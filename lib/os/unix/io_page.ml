(*
 * Copyright (c) 2011-2012 Anil Madhavapeddy <anil@recoil.org>
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

(* A page suitable for IO operations. Represented on UNIX as a normal string,
 * but may also be a Bigarray in the near future *)
type page = {
  pbuf: string;
  poff: int;
  plen: int;
}

(* Page allocation *)

let free_list = Queue.create ()

let page_size = 4096

let alloc () =
  let pbuf = String.create page_size in
  let page = { pbuf; poff=0; plen=page_size } in
  Queue.add page free_list

let get () =
  let rec inner () =
    try
      Queue.pop free_list
    with Queue.Empty -> begin
      alloc ();
      inner ()
    end
  in
  inner ()

let rec get_n = function
  | 0 -> []
  | n -> get () :: (get_n (n - 1))

let put page = 
  Queue.add page free_list

let with_page f =
  let a = get () in
  try_lwt
    lwt res = f a in
    put a;
    return res
  with exn -> begin
    put a;
    fail exn
  end

let with_pages n f =
  let pages = get_n n in
  try_lwt
    lwt res = f pages in
    List.iter put pages;
    return res
  with exn -> begin
    List.iter put pages;
    fail exn
  end

(* View management *)

(* A view takes a slice of a page, and all addressing is relative to 
 * the * page offsets *)
type view = {
  page: page;
  off: int;
  len: int;
}

let get_view page =
  { page; off=0; len=page_size }

(* Narrow the view by off bytes, return a smaller view.
 * If len is provided, then it be *)
let get_subview view n =
  let off = view.off + n in
  let len = view.len - n in
  { page=view.page; off; len }

let get_superview view n =
  let off = view.off - n in
  let len = view.len + n in
  { page=view.page; off; len }

(* Set view length to new value *)
let set_view_len view len =
  { view with len=len }

let add_view_len view len =
  { view with len=view.len + len }

let get_view_len view = view.len

let to_bitstring view =
  let buf = view.page.pbuf in
  let off = (view.page.poff + view.off) lsl 3 in
  let len = view.len lsl 3 in
  buf, off, len

let to_substring view =
  let buf = view.page.pbuf in
  let off = view.page.poff + view.off in
  let len = view.len in
  buf, off, len

