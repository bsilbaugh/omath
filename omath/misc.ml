(****

 Misc

 Misc utilities neither covered by the standard library nor reliable third
 party libraries.

 Copyright 2014 Benjamin Silbaugh

 See LICENSE file for modification and redistribution permissions.

 ****)

module Strng : sig

  val find : string -> string -> int option

  val split : string -> string -> string list

end = struct

  let is_substr_at offset sa sb = 
	let n = String.length sa in
	let m = String.length sb in
	let rec substr_match i = 
	  if i >= n then
		true
	  else
		(sa.[i] = sb.[i + offset]) && substr_match (i + 1)
	in
	if offset + n > m then
	  false
	else
	  substr_match 0

  let rec find_from offset sa sb = 
	if is_substr_at offset sa sb then
	  Some offset
	else
	  let new_offset = offset + 1 in
	  if new_offset < String.length sb then
		find_from new_offset sa sb
	  else
		None

  let rec split_from offset delim str =
	let dlen = String.length delim in
	let slen = String.length str in
	match find_from offset delim str with
	| Some i -> let w = String.sub str offset (i - offset)    
				in w :: (split_from (i + dlen) delim str)
	| None ->   let w = String.sub str offset (slen - offset) 
				in w :: []

  let find = find_from 0

  let split = split_from 0

end
