(*
 * Copyright (c) 2010-2011 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(** Type-safe OCaml wrapper for single-page Xen I/O rings
    Every ring is a 4K page, with separate types for requests
    and responses. The size of the number of requests is cunningly
    set to a power of 2, so the producer/consumer ints can wrap
    safely without needing to mask them. *)

(* For every ring, we need to have bindings that define the type
   of a request and response, and accessor functions for those
   structs to set fields.
 *)

open Lwt

(* Allocate a new grant entry and initialise a ring using it *)
let alloc fn domid =
  lwt gnt = Gnttab.get_free_entry () in
  let page = Gnttab.page gnt in
  let ring = fn page in
  let perm = Gnttab.RW in
  Gnttab.grant_access ~domid ~perm gnt;
  return (gnt, ring)

(* Module type of any request/response ring *)
module type RING = sig
  type idx = int                        (* Ring index *)
  type req                              (* Request *)
  type res                              (* Response *)
  type fring                            (* Front end ring *)

  val alloc: int -> (Gnttab.r * fring) Lwt.t (* Allocate a ring *)
  val req_idx: fring -> int             (* Request position *)
  val pending_responses: fring -> int   (* Pending responses *)
  val free_requests: fring -> int       (* Available req slots *)
  val max_requests: fring -> int        (* Max req slots *)

  val write: fring -> req -> bool       (* Write request to ring *)
  val writev: fring -> req list -> bool (* Write requests to ring *)
  val readv: fring -> (idx -> res -> unit) -> unit (* Read responses from ring *)
end

(* A bounded queue model that will block after a max
   number of requests. There are a fixed number of slots
   available, each with a grant id associated with it.
   A push to a ring will 'use up' an id, and a wakener
   will be called with the response when it's done. *)
module Bounded_ring (Ring:RING) = struct

  type idx = int

  type t = {
    fring: Ring.fring;
    push_waiters: unit Lwt.u Lwt_sequence.t;
    response_waiters: Ring.res Lwt.u option array;
  }

  let t ~backend_domid = 
    lwt gnt, fring = Ring.alloc backend_domid in
    let max_reqs = Ring.max_requests fring in
    let push_waiters = Lwt_sequence.create () in
    let response_waiters = Array.create max_reqs None in
    let r =  { fring; push_waiters; response_waiters } in
    return (gnt, r)

  let rec write t ~evtchn req =
    if Ring.free_requests t.fring > 0 then begin
      let th, u = Lwt.task () in
      let id = Ring.req_idx t.fring in
      t.response_waiters.(id) <- Some u;
      if Ring.write t.fring req then
        Evtchn.notify evtchn;
      th
    end else begin
      let th, u = Lwt.task () in
      let node = Lwt_sequence.add_r u t.push_waiters in
      Lwt.on_cancel th (fun _ -> Lwt_sequence.remove node);
      th >>
      write t ~evtchn req
    end

  (* Check for any responses and activate wakeners as they come in *)
  let poll t =
    Ring.readv t.fring 
      (fun id res ->
        match t.response_waiters.(id) with
        |None -> ()
        |Some u ->
          t.response_waiters.(id) <- None;
          Lwt.wakeup u res
      );
    match Lwt_sequence.take_opt_l t.push_waiters with
    |None -> ()
    |Some u -> Lwt.wakeup u ()

  let max_requests t = Ring.max_requests t.fring
end

module Netif = struct

  module Rx = struct

    module Res = struct
      (* off, flags, status *)
      type raw = (int * int * int)

      type flags = {
        checksum_blank: bool;
        data_validated: bool;
        more_data: bool;
        extra_info: bool;
      }

      type status =
      |Size of int
      |Err of int

      type t = {
        off: int;
        flags: flags;
        status: status;
      }

      let t_of_raw (off, fbits, code) =
        let flags = {
          checksum_blank = (fbits land 0b1 > 0);
          data_validated = (fbits land 0b10 > 0);
          more_data = (fbits land 0b100 > 0);
          extra_info = (fbits land 0b1000 > 0);
        } in
        let status = if code < 0 then Err code else Size code in
        {off; flags; status}
    end

    type id = int
    type req = Gnttab.num
    type res = Res.t

    type sring = Istring.Raw.t  (* shared ring *)
    type fring                  (* front end ring *)

    (* Initialise the shared ring and return a front ring *)
    external init: sring -> fring = "caml_netif_rx_init"

    (* Push a list of requests, bool return indicates notify needed *)
    external writev: fring -> req list -> bool = "caml_netif_rx_request"

    (* Read a list of responses to the request *)
    external raw_readv: fring -> (id -> Res.raw -> unit) -> unit = "caml_netif_rx_response"

    (* Number of responses pending *)
    external pending_responses: fring -> int = "caml_netif_rx_pending_responses" "noalloc"

    (* Maximum number of requests *)
    external max_requests: fring -> int = "caml_netif_rx_max_requests" "noalloc"

    (* Number of free requests *)
    external free_requests: fring -> int = "caml_netif_rx_free_requests" "noalloc"

    let alloc = alloc init
    let readv fring fn = raw_readv fring (fun id res -> fn id (Res.t_of_raw res))

  end

  module Tx = struct

    module Req = struct
      type gso_type = GSO_none | GSO_TCPv4
      type gso = { gso_size: int; gso_type: gso_type; gso_features: int }
      type extra = 
      |GSO of gso
      |Mcast_add of string (* [6] *)
      |Mcast_del of string (* [6] *)

      (* TODO: NETTXF_*: flags as a variant *)
      type flags = int

      type normal = {
        gref: Gnttab.num;
        offset: int;
        flags: flags;
        size: int;
      }

      type t =
      |Normal of normal
      |Extra of extra
    end 

    module Res = struct
      type status = 
      |Dropped
      |Error
      |OK
      |Null

      (* id * status_code *)
      type raw = int 

      type t = status

      let status_of_code = function
      |(-2) -> Dropped
      |(-1) -> Error
      |0 -> OK
      |_ -> Null

      let t_of_raw code =
        status_of_code code
    end

    type idx = int
    type req = Req.t
    type res = Res.t

    type sring = Istring.Raw.t
    type fring 

    external init: sring -> fring = "caml_netif_tx_init"
    external writev: fring -> Req.t list -> bool = "caml_netif_tx_request_n"
    external write: fring -> Req.t -> bool = "caml_netif_tx_request_1"
    external raw_readv: fring -> (idx -> Res.raw -> unit) -> unit = "caml_netif_tx_response"
    external req_idx: fring -> int = "caml_netif_tx_req_idx" "noalloc"
    external pending_responses: fring -> int = "caml_netif_tx_pending_responses" "noalloc"
    external max_requests: fring -> int = "caml_netif_tx_max_requests" "noalloc"
    external free_requests: fring -> int = "caml_netif_tx_free_requests" "noalloc"
    let alloc = alloc init
    let readv fring fn = raw_readv fring (fun id raw -> fn id (Res.t_of_raw raw))
  end

  module Tx_t = Bounded_ring(Tx)
end

module Blkif = struct

  type vdev = int

  module Req = struct
    type op =
      |Read |Write |Write_barrier |Flush

    type seg = {
      gref: Gnttab.num;
      first_sector: int;
      last_sector: int;
    }

    type t = {
      op: op;
      handle: vdev;
      sector: int64;
      segs: seg array;
    }
  end

  module Res = struct
    type status =
      |OK |Error |Not_supported |Unknown of int

    (* id * op * BLKIF_RSP *)
    type raw = (Req.op * int)

    type t = {
      op: Req.op;
      status: status;
    }

    let t_of_raw (op, rsp) =
      let status = match rsp with
      |0 -> OK
      |(-1) -> Error
      |(-2) -> Not_supported
      |x -> Unknown x in
      { op; status }
  end

  type idx = int
  type req = Req.t
  type res = Res.t

  type sring = Istring.Raw.t
  type fring 

  external init: sring -> fring = "caml_blkif_init"
  external writev: fring -> Req.t list -> bool = "caml_blkif_request_n"
  external write: fring -> Req.t -> bool = "caml_blkif_request_1"
  external raw_readv: fring -> (idx -> Res.raw -> unit) -> unit = "caml_blkif_response"
  external pending_responses: fring -> int = "caml_blkif_pending_responses" "noalloc"
  external max_requests: fring -> int = "caml_blkif_max_requests" "noalloc"
  external free_requests: fring -> int = "caml_blkif_free_requests" "noalloc"
  external req_idx: fring -> int = "caml_blkif_req_idx" "noalloc"

  let alloc = alloc init
  let readv fring fn = raw_readv fring (fun idx raw -> fn idx (Res.t_of_raw raw))
end

module Blkif_t = Bounded_ring(Blkif)

(* Raw ring handling section *)

(* Allocate a new grant entry for a raw ring *)
let alloc_raw zero domid =
  lwt gnt = Gnttab.get_free_entry () in
  let page = Gnttab.page gnt in
  zero page;
  let perm = Gnttab.RW in
  Gnttab.grant_access ~domid ~perm gnt;
  return (gnt, page)

module Console = struct
    type t = Istring.Raw.t
    let initial_grant_num : Gnttab.num = 2
    external start_page: unit -> t = "caml_console_start_page"
    external zero: Istring.Raw.t -> unit = "caml_console_ring_init"
    external unsafe_write: t -> string -> int -> int = "caml_console_ring_write"
    external unsafe_read: t -> string -> int -> int = "caml_console_ring_read"
    let alloc domid = alloc_raw zero domid
    let alloc_initial () =
      let num = initial_grant_num in
      let page = start_page () in
      let gnt = Gnttab.alloc ~page num in
      gnt, page
end

module Xenstore = struct
    type t = Istring.Raw.t
    let initial_grant_num : Gnttab.num = 1
    external start_page: unit -> t = "caml_xenstore_start_page"
    external zero: Istring.Raw.t -> unit = "caml_xenstore_ring_init"
    external unsafe_write: t -> string -> int -> int = "caml_xenstore_ring_write"
    external unsafe_read: t -> string -> int -> int = "caml_xenstore_ring_read"
    let alloc_initial () =
      let num = initial_grant_num in
      let page = start_page () in
      zero page;
      let gnt = Gnttab.alloc ~page num in
      gnt, page
end
