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

open Lwt
open Printf
open Nettypes

type proto = [ `ICMP | `TCP | `UDP ]

type classify =
  |Broadcast
  |Gateway
  |Local

exception No_route_to_destination_address of ipv4_addr

type t = {
  ethif: Ethif.t;
  mutable ip: ipv4_addr;
  mutable netmask: ipv4_addr;
  mutable gateways: ipv4_addr list;
  mutable icmp: ipv4_addr -> Bitstring.t -> unit Lwt.t;
  mutable udp: src:ipv4_addr -> dst:ipv4_addr -> Bitstring.t -> unit Lwt.t;
  mutable tcp: src:ipv4_addr -> dst:ipv4_addr -> Bitstring.t -> unit Lwt.t;
}

let is_local t ip =
  let ipand a b = Int32.logand (ipv4_addr_to_uint32 a) (ipv4_addr_to_uint32 b) in
  (ipand t.ip t.netmask) = (ipand ip t.netmask)

let destination_mac t = function
  |ip when ip = ipv4_broadcast || ip = ipv4_blank -> (* Broadcast *)
     return ethernet_mac_broadcast
  |ip when is_local t ip -> (* Local *)
     Ethif.query_arp t.ethif ip
  |ip -> begin (* Gateway *)
     match t.gateways with 
     | hd :: _ -> Ethif.query_arp t.ethif hd
     | [] -> 
        printf "IP.output: no route to %s\n%!" (ipv4_addr_to_string ip);
        fail (No_route_to_destination_address ip)
  end

let ether_header = 48+48+16
let ipv4_header = 4+4+8+16+16+3+13+8+8+16+32+32
let ipv4_csum = 48+48+16+4+4+8+16+16+3+13+8+8

(* Obtain an Ethernet write buffer by looking up the destination
 * desired. *)
let writebuf t ~proto ~dest_ip =
  let page = OS.Io_page.get () in
  let view = OS.Io_page.get_view page in
  let bs = OS.Io_page.to_bitstring view in
  let proto = match proto with |`ICMP -> 1 |`TCP -> 6 |`UDP -> 17 in
  let src = ipv4_addr_to_uint32 t.ip in
  let dst = ipv4_addr_to_uint32 dest_ip in
  let ttl = 38 in (* TODO TTL *)
  lwt dmac = destination_mac t dest_ip >|= ethernet_mac_to_bytes in
  let smac = ethernet_mac_to_bytes (Ethif.mac t.ethif) in
  let _, foff, flen = BITSTRING { dmac:48:string; smac:48:string;
    0x0800:16; 4:4; 5:4; 0:8; 0:16; 0:16; 0:3;
    0:13; ttl:8; proto:8; 0:16; src:32; dst:32 } bs in
  let flen_bytes = flen lsr 3 in
  let appview = OS.Io_page.get_subview view flen_bytes in
  (* Return the application area (appbs) *)
  return appview
 
(* Assume the output has been previously stamped by the writebuf,
 * so we just adjust the contents here. *)
let output t appview =
  (* Work backwards and generate the upper-level view of the frame as
   * generated in get_writebuf above *)
  let view = OS.Io_page.get_superview appview ((ether_header + ipv4_header) lsr 3) in
  let (framebuf, frameoff, framelen) as frame = OS.Io_page.to_bitstring view in
  (* Adjust fields *)
  let ihl = 5 in (* No IP options support yet *)
  let ihl_bits = ihl * 4 * 8 in
  let tlen = (framelen - (48+48+16)) lsr 3 in
  let ipid = Random.int 65535 in (* TODO support ipid *)
  (* Adjust the IPv4 headers with checksum and such *)
  let adj_bs = Bitstring.subbitstring frame (48+48+16+4+4+8) 0 in
  let _ = BITSTRING { tlen:16; ipid:16 } adj_bs in
  let ipv4_header = framebuf, (frameoff + (48+48+16)), ihl_bits in
  let checksum_bs = framebuf, ipv4_csum, 16 in
  let _ = BITSTRING { 0:16 } checksum_bs in
  let checksum = Checksum.ones_complement ipv4_header in
  let _ = BITSTRING { checksum:16 } checksum_bs in
  Ethif.output t.ethif view

let input t pkt =
  bitmatch pkt with
  |{4:4; ihl:4; tos:8; tlen:16; ipid:16;
    flags:3; fragoff:13; ttl:8; proto:8; checksum:16;
    src:32:bind(ipv4_addr_of_uint32 src);
    dst:32:bind(ipv4_addr_of_uint32 dst);
    _ (* options *):(ihl-5)*32:bitstring; data:-1:bitstring } ->

      begin match proto with
      |1 -> (* ICMP *) t.icmp src data
      |6 -> (* TCP *) t.tcp ~src ~dst data
      |17 -> (* UDP *) t.udp ~src ~dst data
      |_ -> return (printf "IPv4: dropping proto %d\n%!" proto)
      end
  |{_} -> return (printf "IPv4: not an IP packet, discarding\n%!")

let default_icmp = fun _ _ -> return ()
let default_udp = fun ~src ~dst _ -> return ()
let default_tcp = fun ~src ~dst _ -> return ()
 
let create ethif = 
  let ip = ipv4_blank in
  let netmask = ipv4_blank in
  let gateways = [] in
  let icmp = default_icmp in
  let udp = default_udp in
  let tcp = default_tcp in
  let t = { ethif; ip; netmask; gateways; icmp; udp; tcp } in
  Ethif.attach ethif (`IPv4 (input t));
  let th,_ = Lwt.task () in
  Lwt.on_cancel th (fun () ->
    printf "IPv4: shutting down\n%!";
    Ethif.detach ethif `IPv4);
  t, th

let attach t = function
  |`ICMP x -> t.icmp <- x
  |`UDP x -> t.udp <- x
  |`TCP x -> t.tcp <- x

let detach t = function
  |`ICMP -> t.icmp <- default_icmp
  |`UDP -> t.udp <- default_udp
  |`TCP -> t.tcp <- default_tcp

let set_ip t ip = 
  t.ip <- ip;
  (* Inform ARP layer of new IP *)
  Ethif.add_ip t.ethif ip

let get_ip t = t.ip

let set_netmask t netmask =
  t.netmask <- netmask;
  return ()

let set_gateways t gateways =
  t.gateways <- gateways;
  return ()

let mac t = Ethif.mac t.ethif

