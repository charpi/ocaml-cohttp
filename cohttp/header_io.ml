(*
 * Copyright (c) 2012-2013 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2011-2012 Martin Jambon <martin@mjambon.com>
 * Copyright (c) 2010 Mika Illouz
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
 *
 *)

let split_header str =
  match Stringext.split ~max:2 ~on:':' str with
  | x::y::[] -> [x; Stringext.trim_left y]
  | x -> x

module Make(IO : S.IO) = struct
  open IO

  module Transfer_IO = Transfer_io.Make(IO)

  let parse ic =
    (* consume also trailing "^\r\n$" line *)
    let rec parse_headers' headers =
      read_line ic >>= function
      |Some "" | None -> return headers
      |Some line -> begin
          match split_header line with
          | [hd;tl] ->
              let header = String.lowercase hd in
              parse_headers' (Header.add headers header tl);
          | _ -> return headers
      end
    in parse_headers' (Header.init ())

  let parse_form headers ic =
    (* If the form is query-encoded, then extract those parameters also *)
    let encoding = Header.get_transfer_encoding headers in
    Transfer_IO.to_string encoding ic >>= fun body ->
    return (Uri.query_of_encoded body)

  let parse_multipart_form headers ic = 
    let content = Header.get headers "content-type" in
    let encoding = Header.get_transfer_encoding headers in
    Transfer_IO.to_string Unknown ic >>= fun body ->
    match content with 
    | None ->
       return ([])
    | Some str ->
       let rec filter_content (str_list :bytes list) =
	 let first = List.hd str_list in
	 if Bytes.length first < 8 then 
	   List.tl str_list
	 else
	   match (Bytes.sub first 0 8) with
	   | "Content-" -> filter_content (List.tl str_list)
	   | _ -> str_list in
       let parse_part part =
	 let lines = Stringext.split ~on:'\n' part in
	 let re = Re_emacs.re ~case:true "Content-Disposition: form-data; name=\"\(.[^\"]*\)\".*" in
	 let subs = Re.exec ~pos:0 (Re.(compile (seq ([re])))) (List.hd lines) in
	 let name = (Re.get subs 1) in
	 let filtered = filter_content lines in
	 (name, filtered) in
       let boundary = Stringext.split ~on:';' str 
		      |> List.tl |> List.hd
		      |> Stringext.split ~on:'='
		      |> List.tl |> List.hd in
       let reg_exp = Re_perl.re (String.concat "" [boundary;"\n"]) in
       let compiled = Re.compile reg_exp in
       let split = Re_pcre.split compiled body in
       let parts = split
		   |> List.tl
		   |> List.map parse_part
       in
       return (parts)
end

