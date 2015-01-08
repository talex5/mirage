(*
 * Copyright (c) 2011-2014 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2013      Thomas Gazagnaire <thomas@gazagnaire.org>
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

module type DEVICE = sig
  (** Device operations.
      Defines the functions to connect and disconnect any device *)

  type +'a io
  (** A potentially blocking I/O operation *)

  type t
  (** The type representing the internal state of the device *)

  type error
  (** An error signalled by the device, normally returned after a
      connection attempt *)

  type id
  (** Type defining an identifier for this device that uniquely
      identifies it among a device tree. *)

  val id : t -> id
  (** Return the identifier that was used to construct this device *)

  val connect: id -> [ `Error of error | `Ok of t ] io
  (** Connect to the device identified by [id] *)

  val disconnect : t -> unit io
  (** Disconnect from the device.  While this might take some
      time to complete, it can never result in an error. *)
end

module type TIME = sig
  (** Time operations for cooperative threads. *)

  type +'a io
  (** A potentially blocking I/O operation *)

  val sleep: float -> unit io
  (** [sleep nsec] Block the current thread for [TODO remove float] *)
end

module type RANDOM = sig
  (** Operations to generate entropy.  This is currently a passthrough to
      the OCaml Random generator, and will be deprecated in V2 and turned
      into a proper DEVICE with blocking modes. *)

  val self_init : unit -> unit
  (** Initialize the generator with a random seed chosen in a system-dependent way. *)

  val int : int -> int
  (** [int bound] returns a random integer between 0 (inclusive) and [bound] (exclusive).
     [bound] must be greater than 0 and less than 2{^30}. *)

  val int32 : int32 -> int32
  (** [int32 bound] returns a random integer between 0 (inclusive) and [bound] (exclusive).
      [bound] must be greater than 0. *)
end

module type ENTROPY = sig
  (** A native entropy provider. **)

  type error = [
    | `No_entropy_device of string
  ]
  (** Represents errors when attaching the entropy provider. *)

  include DEVICE with
    type error := error

  type buffer
  (** usually a cstruct *)

  type handler = source:int -> buffer -> unit
  (**
   * A [handler] is called whenever the system has extra entropy to announce.
   * No guarantees are made about the entropy itself, other than it being
   * environmentally derived. In particular, the amount of entropy in the buffer
   * can be far lower than the size of the [buffer].
   *
   * [source] is a small integer, describing the provider but with no other
   * meaning.
   *
   * [handler] is expected to return quickly.
   * *)

  val handler : t -> handler -> unit io
  (** [handler h] registers the single global [handler] that will receive
   * entropy. There might be additional, provider-specific blocking semantics.
   * *)

end

module type CLOCK = sig
  (** Clock operations.
      Currently read-only to retrieve the time in various formats. *)

  type tm =
    { tm_sec : int;               (** Seconds 0..60 *)
      tm_min : int;               (** Minutes 0..59 *)
      tm_hour : int;              (** Hours 0..23 *)
      tm_mday : int;              (** Day of month 1..31 *)
      tm_mon : int;               (** Month of year 0..11 *)
      tm_year : int;              (** Year - 1900 *)
      tm_wday : int;              (** Day of week (Sunday is 0) *)
      tm_yday : int;              (** Day of year 0..365 *)
      tm_isdst : bool;            (** Daylight time savings in effect *)
    }
  (** The type representing wallclock time and calendar date. *)

  val time : unit -> float
  (** Return the current time since 00:00:00 GMT, Jan. 1, 1970, in
      seconds. *)

  val gmtime : float -> tm
  (** Convert a time in seconds, as returned by {!time}, into a
      date and a time. Assumes UTC (Coordinated Universal Time), also
      known as GMT. *)
end

module type FLOW = sig
  (** A connection between endpoints. *)

  type +'a io
  (** A potentially blocking I/O operation *)

  type buffer
  (** Abstract type for a memory buffer that may not be page aligned. *)

  type flow
  (** A flow represents the state of a single stream that is connected
      to an endpoint. *)

  type error

  val read : flow -> [`Ok of buffer | `Eof | `Error of error ] io
  (** [read flow] will block until it either successfully reads a segment
      of data from the current flow, receives an [Eof] signifying that
      the connection is now closed, or an [Error]. *)

  val write : flow -> buffer -> [`Ok of unit | `Eof | `Error of error ] io
  (** [write flow buffer] will block until [buffer] has been added to the
      send queue. There is no indication when the buffer has actually been
      read and, therefore, it must not be reused.
      The contents may be transmitted in separate packets, depending on the
      underlying transport. The result [`Ok ()] indicates success, [`Eof]
      indicates that the connection is now closed and [`Error] indicates some
      other error. *)

  val writev : flow -> buffer list -> [`Ok of unit | `Eof | `Error of error ] io
  (** [writev flow buffers] will block until the buffers have all been added
      to the send queue. There is no indication when the buffers have actually
      been read and, therefore, they must not be reused.
      The result [`Ok ()] indicates success, [`Eof] indicates that the
      connection is now closed and [`Error] indicates some other error. *)

  val close : flow -> unit io
  (** [close flow] will flush all pending writes and signal the end of the
      flow to the remote endpoint.  When the result [unit io] becomes
      determined, all further calls to [read flow] will result in a [`Eof]. *)
end

module type CONSOLE = sig
  (** Text console input/output operations. *)

  type error = [
    | `Invalid_console of string
  ]
  (** The type representing possible errors when attaching a console. *)

  include DEVICE with
    type error := error

  include FLOW with
      type error  := error
  and type 'a io  := 'a io
  and type flow   := t

  val log : t -> string -> unit
  (** [log str] writes as much characters of [str] that can be written
      in one write operation to the console [t], then writes
      "\r\n" to it. *)

  val log_s : t -> string -> unit io
  (** [log_s str] is a thread that writes [str ^ "\r\n"] in the
      console [t]. *)

end

module type BLOCK = sig
  (** Operations on sector-addressible block devices, usually used
      for persistent storage *)

  type page_aligned_buffer
  (** Abstract type for a page-aligned memory buffer *)

  type error = [
    | `Unknown of string (** an undiagnosed error *)
    | `Unimplemented     (** operation not yet implemented in the code *)
    | `Is_read_only      (** you cannot write to a read/only instance *)
    | `Disconnected      (** the device has been previously disconnected *)
  ]
  (** IO operation errors *)


  include DEVICE with
    type error := error

  type info = {
    read_write: bool;    (** True if we can write, false if read/only *)
    sector_size: int;    (** Octets per sector *)
    size_sectors: int64; (** Total sectors per device *)
  }
  (** Characteristics of the block device. Note some devices may be able
      to make themselves bigger over time. *)

  val get_info: t -> info io
  (** Query the characteristics of a specific block device *)

  val read: t -> int64 -> page_aligned_buffer list -> [ `Error of error | `Ok of unit ] io
  (** [read device sector_start buffers] returns a blocking IO operation which
      attempts to fill [buffers] with data starting at [sector_start].
      Each of [buffers] must be a whole number of sectors in length. The list
      of buffers can be of any length. *)

  val write: t -> int64 -> page_aligned_buffer list -> [ `Error of error | `Ok of unit ] io
  (** [write device sector_start buffers] returns a blocking IO operation which
      attempts to write the data contained within [buffers] to [t] starting
      at [sector_start]. When the IO operation completes then all writes have been
      persisted.

      Once submitted, it is not possible to cancel a request and there is no timeout.

      The operation may fail with
      * [`Unimplemented]: the operation has not been implemented, no data has been written
      * [`Is_read_only]: the device is read-only, no data has been written
      * [`Disconnected]: the device has been disconnected at application request,
        an unknown amount of data has been written
      * [`Unknown]: some other permanent, fatal error (e.g. disk is on fire), where
        an unknown amount of data has been written

      Each of [buffers] must be a whole number of sectors in length. The list
      of buffers can be of any length.

      The data will not be copied, so the supplied buffers must not be re-used
      until the IO operation completes. *)

end

module type NETWORK = sig
  (** A network interface that serves Ethernet frames. *)

  type page_aligned_buffer
  (** Abstract type for a page-aligned memory buffer *)

  type buffer
  (** Abstract type for a memory buffer that may not be page aligned *)

  type error = [
    | `Unknown of string (** an undiagnosed error *)
    | `Unimplemented     (** operation not yet implemented in the code *)
    | `Disconnected      (** the device has been previously disconnected *)
  ]
  (** IO operation errors *)

  type macaddr
  (** Unique MAC identifier for the device *)

  include DEVICE with
    type error := error

  val write : t -> buffer -> unit io
  (** [write nf buf] outputs [buf] to netfront [nf]. *)

  val writev : t -> buffer list -> unit io
  (** [writev nf bufs] output a list of buffers to netfront [nf] as a
      single packet. *)

  val listen : t -> (buffer -> unit io) -> unit io
  (** [listen nf fn] is a blocking operation that calls [fn buf] with
      every packet that is read from the interface.  It returns as soon
      as it has initialised, and the function can be stopped by calling
      [disconnect] in the device layer. *)

  val mac : t -> macaddr
  (** [mac nf] is the MAC address of [nf]. *)

  type stats = {
    mutable rx_bytes : int64;
    mutable rx_pkts : int32;
    mutable tx_bytes : int64;
    mutable tx_pkts : int32;
  }
  (** Frame statistics to track the usage of the device. *)

  val get_stats_counters : t -> stats
  (** Obtain the most recent snapshot of the device statistics. *)

  val reset_stats_counters : t -> unit
  (** Reset the statistics associated with this device to their defaults. *)
end

module type ETHIF = sig
  (** An Ethernet stack that parses frames from a network device and
      can associate them with IP address via ARP. *)

  type buffer
  (** Abstract type for a memory buffer that may not be page aligned *)

  type netif
  (** Abstract type for an Ethernet network interface. *)

  type error = [
    | `Unknown of string (** an undiagnosed error *)
    | `Unimplemented     (** operation not yet implemented in the code *)
    | `Disconnected      (** the device has been previously disconnected *)
  ]
  (** IO operation errors *)

  type macaddr
  (** Unique MAC identifier for the device *)

  include DEVICE with
        type error := error
    and type id    := netif

  val write : t -> buffer -> unit io
  (** [write nf buf] outputs [buf] to netfront [nf]. *)

  val writev : t -> buffer list -> unit io
  (** [writev nf bufs] output a list of buffers to netfront [nf] as a
      single packet. *)

  val mac : t -> macaddr
  (** [mac nf] is the MAC address of [nf]. *)

  val input :
    arpv4:(buffer -> unit io) -> ipv4:(buffer -> unit io) -> ipv6:(buffer -> unit io) -> t -> buffer -> unit io
  (** FIXME [listen nf fn] is a blocking operation that calls [fn buf] with
      every packet that is read from the interface.  It returns as soon
      as it has initialised, and the function can be stopped by calling
      [disconnect] in the device layer. *)
end

module type IP = sig
  (** An IP stack that parses Ethernet frames into IP packets *)

  type buffer
  (** Abstract type for a memory buffer that may not be page aligned *)

  type ethif
  (** Abstract type for an Ethernet device. *)

  type ipaddr
  (** Abstract type for an IP address. *)

  type prefix
  (** Abstract type for an IP prefix. *)

  type error = [
    | `Unknown of string (** an undiagnosed error *)
    | `Unimplemented     (** operation not yet implemented in the code *)
  ]
  (** IO operation errors *)

  include DEVICE with
        type error := error
    and type id    := ethif

  type callback = src:ipaddr -> dst:ipaddr -> buffer -> unit io
  (** An input continuation used by the parsing functions to pass on
      an input packet down the stack.
      [callback ~src ~dst buf] will be called with [src] and [dst]
      containing the source and destination IP address respectively,
      and [buf] will be a buffer pointing at the start of the IP
      payload. *)

  val input:
    t ->
    tcp:callback -> udp:callback -> default:(proto:int -> callback) ->
    buffer -> unit io
  (** [input ~tcp ~udp ~default ip buf] demultiplexes an incoming [buffer] that
      contains an IP frame.  It examines the protocol header and passes the result
      onto either the [tcp] or [udp] function, or the [default] function for
      unknown IP protocols. *)

  val allocate_frame: t -> dst:ipaddr -> proto:[`ICMP | `TCP | `UDP] -> buffer * int
  (** [allocate_frame t ~dst ~proto] retrurns a pair [(pkt, len)] such that
      [Cstruct.sub pkt 0 len] is the IP header (including the link layer part) of a
      packet going to [dst] for protocol [proto].  The space in [pkt] after the
      first [len] bytes can be used by the client. *)

  val write: t -> buffer -> buffer -> unit io
  (** [write t frame buf] writes the packet [frame :: buf :: []] to the
      address [dst]. *)

  val writev: t -> buffer -> buffer list -> unit io
  (** [writev t frame bufs] writes the packet [frame :: bufs]. *)

  val checksum : buffer -> buffer list -> int
  (** [checksum frame bufs] computes the IP checksum of [bufs] computing the
      pseudo-header from the actual header [frame].  It assumes that frame is of
      the form returned by [allocate_frame], i.e., that it contains the link-layer
      part. *)

  val get_source : t -> dst:ipaddr -> ipaddr
  (** [get_source ip ~dst] is the source address to be used to send a packet to
      [dst]. *)

  val set_ip: t -> ipaddr -> unit io
  (** Set the IP address associated with this interface.  For IPv4, currently
      only supports a single IPv4 address, and aliases will be added in a future
      revision. *)

  val get_ip: t -> ipaddr list
  (** Get the IP addresses associated with this interface. *)

  val set_ip_netmask: t -> prefix -> unit io
  (** Set an IP netmask associated with this interface.  For IPv4, currently
      only supports a single IPv4 netmask, and aliases will be added in a future
      revision. *)

  val get_ip_netmasks: t -> prefix list
  (** Get the IP netmasks associated with this interface. *)

  val set_ip_gateways: t -> ipaddr list -> unit io
  (** Set an IP gateways associated with this interface. *)

  val get_ip_gateways: t -> ipaddr list
  (** Get the IP gateways associated with this interface. *)
end

module type IPV4 = sig
  include IP
  val input_arpv4: t -> buffer -> unit io
end

module type IPV6 = sig
  include IP
end

module type UDP = sig
  (** A UDP stack that can send and receive datagrams. *)

  type buffer
  (** Abstract type for a memory buffer that may not be page aligned. *)

  type ip
  (** Abstract type for an IPv4/6 stack for this stack to connect to. *)

  type ipaddr
  (** Abstract type for an IP address representation. *)

  type ipinput
  (** An input function continuation to pass onto the underlying {!ipv4}
      stack.  This will normally be a NOOP for a conventional kernel, but
      a direct implementation will parse the buffer. *)

  type error = [
    | `Unknown of string (** an undiagnosed error *)
  ]
  (** IO operation errors *)

  include DEVICE with
      type error := error
  and type id := ip

  type callback = src:ipaddr -> dst:ipaddr -> src_port:int -> buffer -> unit io
  (** Callback function that adds the UDP metadata for [src] and [dst] IP
      addresses, the [src_port] of the connection and the [buffer] payload
      of the datagram. *)

  val input: listeners:(dst_port:int -> callback option) -> t -> ipinput
  (** [input listeners t] demultiplexes incoming datagrams based on their destination
      port.  The [listeners] callback is will either return a concrete handler or
      a [None], which results in the datagram being dropped. *)

  val write: ?source_port:int -> dest_ip:ipaddr -> dest_port:int -> t -> buffer -> unit io
  (** [write ~source_port ~dest_ip ~dest_port udp data] is a thread that
      writes [data] from an optional [source_port] to a [dest_ip] and [dest_port]
      IPv4 address pair. *)
end

module type TCP = sig
  (** A TCP stack that can send and receive reliable streams using the TCP protocol. *)

  type buffer
  (** Abstract type for a memory buffer that may not be page aligned. *)

  type ip
  (** Abstract type for an IPv4 stack for this stack to connect to. *)

  type ipaddr
  (** Abstract type for an IPv4 address representation. *)

  type ipinput
  (** An input function continuation to pass onto the underlying {!ipv4}
      stack.  This will normally be a NOOP for a conventional kernel, but
      a direct implementation will parse the buffer. *)

  type flow
  (** A flow represents the state of a single TCPv4 stream that is connected
      to an endpoint. *)

  type error = [
    | `Unknown of string (** an undiagnosed error. *)
    | `Timeout  (** connection attempt did not get a valid response. *)
    | `Refused  (** connection attempt was actively refused via an RST. *)
  ]
  (** IO operation errors *)

  include DEVICE with
      type error := error
  and type id := ip

  include FLOW with
      type error  := error
  and type 'a io  := 'a io
  and type buffer := buffer
  and type flow   := flow

  type callback = flow -> unit io
  (** Application callback that receives a [flow] that it can read/write to. *)

  val get_dest : flow -> ipaddr * int
  (** Get the destination IPv4 address and destination port that a flow is
      currently connected to. *)

  val write_nodelay : flow -> buffer -> unit io
  (** [write_nodelay flow] will block until the contents of [buffer list]
      are all successfully transmitted to the remote endpoint. Buffering
      within the stack is minimized in this mode.  Note that this API will
      change in a future revision to be a per-flow attribute instead of a
      separately exposed function. *)

  val writev_nodelay : flow -> buffer list -> unit io
  (** [writev_nodelay flow] will block until the contents of [buffer list]
      are all successfully transmitted to the remote endpoint. Buffering
      within the stack is minimized in this mode.  Note that this API will
      change in a future revision to be a per-flow attribute instead of a
      separately exposed function. *)

  val create_connection : t -> ipaddr * int ->
    [ `Ok of flow | `Error of error ] io
  (** [create_connection t (addr,port)] will open a TCPv4 connection to the
      specified endpoint. *)

  val input: t -> listeners:(int -> callback option) -> ipinput
  (** [input t listeners] defines a mapping of threads that are willing to
      accept new flows on a given port.  If the [callback] returns [None],
      the input function will return an RST to refuse connections on a port. *)
end

module type STACKV4 = sig
  (** A complete TCP/IPv4 stack that can be used by applications to receive
      and transmit network traffic. *)

  type console
  (** Abstract type of a console logger. *)

  type netif
  (** Abstract type of a network interface that is used to transmit and receive
      traffic associated with this stack. *)

  type mode
  (** Abstract type of the configuration modes associated with this interface.
      These can consist of the IPv4 address binding, or a DHCP interface. *)

  type ('console, 'netif, 'mode) config
  (** Abstract type for the collection of user configuration specified to
      construct a stack. *)

  type ipv4addr
  (** Abstract type of an IPv4 address *)

  type buffer
  (** Abstract type for a memory buffer that may not be page aligned. *)

  type udpv4
  (** Abstract type for a UDPv4 stack. *)

  type tcpv4
  (** Abstract type for a TCPv4 stack. *)

  type ipv4
  (** Abstract type for a IPv4 stack *)

  type error = [
    | `Unknown of string
  ]
  (** I/O operation errors *)

  include DEVICE with
    type error := error
    and type id = (console, netif, mode) config

  module UDPV4 : UDP
    with type +'a io = 'a io
     and type ipaddr = ipv4addr
     and type buffer = buffer
     and type t = udpv4
     and type ip = ipv4

  module TCPV4 : TCP
    with type +'a io = 'a io
     and type ipaddr = ipv4addr
     and type buffer = buffer
     and type t = tcpv4
     and type ip = ipv4

  module IPV4 : IPV4
    with type +'a io = 'a io
     and type ipaddr = ipv4addr
     and type buffer = buffer
     and type t = ipv4

  val udpv4 : t -> udpv4
  (** [udpv4 t] obtains a descriptor for use with the [UDPV4] module,
      usually to transmit traffic. *)

  val tcpv4 : t -> tcpv4
  (** [tcpv4 t] obtains a descriptor for use with the [TCPV4] module,
      usually to initiate outgoing connections. *)

  val ipv4 : t -> ipv4
  (** [ipv4 t] obtains a descriptor for use with the [IPV4] module,
      which can handle raw IPv4 frames, or manipulate IP address
      configuration on the stack interface. *)

  val listen_udpv4 : t -> port:int -> UDPV4.callback -> unit
  (** [listen_udpv4 t ~port cb] will register the [cb] callback on
      the UDPv4 [port] and immediately return.  Multiple bindings
      to the same port will overwrite previous bindings, so callbacks
      will not chain if ports clash. *)

  val listen_tcpv4 : t -> port:int -> TCPV4.callback -> unit
  (** [listen_tcpv4 t ~port cb] will register the [cb] callback on
      the TCPv4 [port] and immediately return.  Multiple bindings
      to the same port will overwrite previous bindings, so callbacks
      will not chain if ports clash. *)

  val listen : t -> unit io
  (** [listen t] will cause the stack to listen for traffic on the
      network interface associated with the stack, and demultiplex
      traffic to the appropriate callbacks. *)
end

module type CHANNEL = sig
  (** Type of a buffered byte-stream that is attached to an unbuffered
      flow (e.g. a TCPv4 connection). *)

  type buffer
  (** Abstract type for a memory buffer that may not be page aligned. *)

  type flow
  (** Abstract type for an unbuffered network flow. *)

  type t
  (** State associated with this channel, such as the inflight buffers. *)

  type +'a io
  (** Abstract type of a blocking IO monad. *)

  type 'a io_stream
  (** Abstract type of a blocking stream of IO requests. *)

  val create       : flow -> t
  (** [create flow] will allocate send and receive buffers and associated them
      with the given unbuffered [flow]. *)

  val to_flow      : t -> flow
  (** [to_flow t] will return the flow that backs this channel. *)

  val read_char    : t -> char io
  (** Read a single character from the channel, blocking if there is no immediately
      available input data. *)

  val read_until   : t -> char -> (bool * buffer) io
  (** [read_until t ch] will read from the channel until the given [ch] character
      is found.  It returns a tuple indicating whether the character was found at
      all ([false] indicates that an EOF condition occurred before the character
      was encountered), and the [buffer] pointing to the position immediately
      after the character (or the complete scanned buffer if the character was
      never encountered). *)

  val read_some    : ?len:int -> t -> buffer io
  (* [read_some ?len t] will read up to [len] characters from the input channel
     and at most a full [buffer]. If [len] is not specified, it will read all
     available data and return that buffer. *)

  val read_stream  : ?len:int -> t -> buffer io_stream
  (** [read_stream ?len t] will return up to [len] characters as a stream of
     buffers. This call will probably be removed in a future revision of the API
     in favour of {!read_some}. *)

  val read_line    : t -> buffer list io
  (** [read_line t] will read a line of input, which is terminated either by
     a CRLF sequence, or the end of the channel (which counts as a line).
     @return Returns a list of views that terminates at EOF. *)

  val write_char   : t -> char -> unit
  (** [write_char t ch] writes a single character to the output channel. *)

  val write_string : t -> string -> int -> int -> unit
  (** [write_string t buf off len] writes [len] bytes from a string [buf],
     starting from from offset [off]. *)

  val write_buffer : t -> buffer -> unit
  (** [write_buffer t buf] will copy the buffer to the channel's output buffer.
     The buffer should not be modified after being written, and it will be
     recycled into the buffer allocation pool at some future point. *)

  val write_line   : t -> string -> unit
  (** [write_line t buf] will write the string [buf] to the output channel
     and append a newline character afterwards. *)

  val flush        : t -> unit io
  (** [flush t] will flush the output buffer and block if necessary until it
     is all written out to the flow. *)

  val close        : t -> unit io
  (** [close t] will call {!flush} and then close the underlying flow. *)

end

module type FS = sig
  (** A filesystem module. *)

  (** Abstract type representing an error from the block layer *)
  type block_device_error

  type error = [
    | `Not_a_directory of string             (** Cannot create a directory entry in a file *)
    | `Is_a_directory of string              (** Cannot read or write the contents of a directory *)
    | `Directory_not_empty of string         (** Cannot remove a non-empty directory *)
    | `No_directory_entry of string * string (** Cannot find a directory entry *)
    | `File_already_exists of string         (** Cannot create a file with a duplicate name *)
    | `No_space                              (** No space left on the block device *)
    | `Format_not_recognised of string       (** The block device appears to not be formatted *)
    | `Unknown_error of string
    | `Block_device of block_device_error
  ]

  include DEVICE
    with type error := error

  (** Abstract type for a page-aligned memory buffer *)
  type page_aligned_buffer

  val read: t -> string -> int -> int -> [ `Ok of page_aligned_buffer list | `Error of error ] io
  (** [read t key offset length] reads up to [length] bytes from the value
      associated with [key]. If less data is returned than requested, this
      indicates the end of the value. *)

  val size: t -> string -> [`Error of error | `Ok of int64] io
  (** Get the value size. *)

  (* The following is specific to FS: *)
  (** Per-file/directory statistics *)
  type stat = {
    filename: string; (** Filename within the enclosing directory *)
    read_only: bool;  (** True means the contents are read-only *)
    directory: bool;  (** True means the entity is a directory; false means a file *)
    size: int64;      (** Size of the entity in bytes *)
  }

  (** [format t size] erases the contents of [t] and creates an empty filesystem
      of size [size] bytes *)
  val format: t -> int64 -> [ `Ok of unit | `Error of error ] io

  (** [create t path] creates an empty file at [path] *)
  val create: t -> string -> [ `Ok of unit | `Error of error ] io

  (** [mkdir t path] creates an empty directory at [path] *)
  val mkdir: t -> string -> [ `Ok of unit | `Error of error ] io

  (** [destroy t path] removes a [path] (which may be a file or an empty
      directory) on filesystem [t] *)
  val destroy: t -> string -> [ `Ok of unit | `Error of error ] io

  (** [stat t path] returns information about file or directory at [path] *)
  val stat: t -> string -> [ `Ok of stat | `Error of error ] io

  (** [listdir t path] returns the names of files and subdirectories
      within the directory [path] *)
  val listdir: t -> string -> [ `Ok of string list | `Error of error ] io

  (** [write t path offset data] writes [data] at [offset] in file [path] on
      filesystem [t] *)
  val write: t -> string -> int -> page_aligned_buffer -> [ `Ok of unit | `Error of error ] io

end

module type IO_PAGE = sig
  (** Memory allocation interface. *)

  type buf
  (** Type of a C buffer (usually Cstruct) *)

  type t = private (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
  (** Type of memory blocks. *)

  val get : int -> t
  (** [get n] allocates and returns a memory block of [n] pages. If
      there is not enough memory, the unikernel will terminate. *)

  val get_order : int -> t
  (** [get_order i] is [get (1 lsl i)]. *)

  val pages : int -> t list
  (** [pages n] allocates a memory block of [n] pages and return the the
      list of pages allocated. *)

  val pages_order : int -> t list
  (** [pages_order i] is [pages (1 lsl i)]. *)

  val length : t -> int
  (** [length t] is the size of [t], in bytes. *)

  val to_cstruct : t -> buf
  val to_string : t -> string

  val to_pages : t -> t list
  (** [to_pages t] is a list of [size] memory blocks of one page each,
      where [size] is the size of [t] in pages. *)

  val string_blit : string -> int -> t -> int -> int -> unit
  (** [string_blit src srcoff dst dstoff len] copies [len] bytes from
      string [src], starting at byte number [srcoff], to memory block
      [dst], starting at byte number dstoff. *)

  val blit : t -> t -> unit
  (** [blit t1 t2] is the same as {!Bigarray.Array1.blit}. *)

  val round_to_page_size : int -> int
  (** [round_to_page_size n] returns the number of bytes that will be
      allocated for storing [n] bytes in memory *)
end

module type KV_RO = sig
  (** Static Key/value store. *)

  type error =
    | Unknown_key of string

  include DEVICE
    with type error := error

  (** Abstract type for a page-aligned memory buffer *)
  type page_aligned_buffer

  val read: t -> string -> int -> int -> [ `Ok of page_aligned_buffer list | `Error of error ] io
  (** [read t key offset length] reads up to [length] bytes from the value
      associated with [key]. If less data is returned than requested, this
      indicates the end of the value. *)

  val size: t -> string -> [`Error of error | `Ok of int64] io
  (** Get the value size. *)

end
