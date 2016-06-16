(*
 * Copyright (c) 2011-2015 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2013      Citrix Systems Inc
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

(** A network interface that serves Ethernet frames. *)

open Result

type stats = {
  rx_bytes: int64;
  rx_pkts: int32;
  tx_bytes: int64;
  tx_pkts: int32;
}
(** The type for frame statistics to track the usage of the device. *)

module type V2 = sig

  type buffer
  (** The type for memory buffers. *)

  type error = private [>
    | `Disconnected      (** the device has been previously disconnected *)
  ]
  (** The type for errors *)

  type macaddr
  (** The type for unique MAC identifiers for the device. *)

  type checksum_state
    (** The TCP or UDP checksum's state, which is passed between VMs along with the frame.
        We want to avoid costly checksum calculations where possible. By tracking this state,
        Xen VMs can optimise two common cases:
        - The frame is only sent between VMs (generating and verifying can be skipped)
        - The checksum is handled in hardware by the network card
      *)

  val verified: checksum_state
  (** The TCP/UDP checksum is believed to be correct.
      Either the frame has never been sent over an unreliable network connection, or someone else has checked
      it since it arrived.
      The frame can be consumed without verification. *)

  val unverified: checksum_state
  (** The TCP/UDP checksum should be correct, but has not been tested since the frame arrived over an
      unreliable network transport.
      The checksum should be verified before the packet is consumed. *)

  val partial: t -> checksum_state option
  (** The [partial] flag, when available, indicates that the TCP/UDP checksum
      only covers the pseudo-header, not the payload.
      This means that the frame has never been sent over an unreliable
      network connection. The frame can be consumed without verification, but
      something (probably the NIC) will need to complete the checksum before
      transmission over a physical network. This flag is optional, as not all
      devices support checksum-offload. *)

  include V1.DEVICE with
    type error := error

  val writev: t -> checksum:checksum_state -> buffer list -> (unit, error) result io
  (** [writev t bufs] output a list of buffers to netfront [t] as a
      single frame. *)

  val listen: t -> (checksum:checksum_state -> buffer -> [`Continue] io) -> [`Disconnected] io
  (** [run t fn] is a loop that calls [fn ~checksum frame] with
      every frame that is read from the interface. No further frames
      are consumed until [fn] returns. This can be used to provide
      back-pressure, but care must be taken to avoid deadlocks (e.g.
      if processing a frame requires an RPC over the network). [fn]
      should therefore normally spawn a new thread to handle the frame
      in the background.
      [run] returns only when the device is disconnected.
    *)

  val mac: t -> macaddr
  (** [mac t] is the MAC address of [t]. *)

  val get_stats_counters: t -> stats
  (** Obtain the most recent snapshot of the device statistics. *)

  val reset_stats_counters: t -> unit
  (** Reset the statistics associated with this device to their
      defaults. *)
end

module type V1 = V1.NETWORK
