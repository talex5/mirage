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

type t = {
  page: Bitstring.t;
  mutable detached: bool;
}

let free_list = Queue.create ()

let page_size = 4096

let alloc () =
  let page = Bitstring.bitstring_of_string (String.create page_size) in
  Queue.add page free_list

let get () =
  let rec inner () =
    try
      Queue.pop free_list
    with Queue.Empty -> begin
      alloc ();
      inner ()
    end in
  { page = inner (); detached = false }

let rec get_n = function
  | 0 -> []
  | n -> get () :: (get_n (n - 1))

let return_to_free_list (x: Bitstring.t) =
  (* TODO: assert that the buf is a page aligned one we allocated above *)
  Queue.add x free_list

let put (x: t) =
  if not x.detached then return_to_free_list x.page

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

(*
let detach (x: t) =
  Gc.finalise return_to_free_list x.page;
  x.detached <- true
*)

let to_bitstring (x: t) =
  x.page

let of_bitstring (x: Bitstring.t) = {
  page = x;
  detached = true (* XXX: assume this page has already been detached *)
}

