(****

 File

 High level utilities for reading and writing data to files.

 Copyright 2014 Benjamin Silbaugh

 See LICENSE file for redistribution and modification permissions.

 ****)

(*** Input ***)

val with_input_file : string -> (in_channel -> 'a) -> 'a

val fold_lines : ('a -> string -> 'a) -> 'a -> in_channel -> 'a

val map_lines_to_list : (string -> 'a) -> in_channel -> 'a list

val iter_lines : (string -> unit) -> in_channel -> unit

val read_lines : in_channel -> string list

(*** Output ***)

val with_output_file : string -> (out_channel -> 'a) -> 'a

val write_lines : string list -> out_channel -> unit
