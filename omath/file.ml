(****

 File

 High level utilities for reading and writing data to files.

 Copyright 2014 Benjamin Silbaugh

 See LICENSE file for redistribution and modification permissions.

 ****)

(*** Input Operations ***)

let with_input_file file_name f =
  let chnl = open_in file_name in
  let res = f chnl in
  close_in chnl ;
  res

let rec fold_lines f acc chnl = 
  try
	let line = input_line chnl in
	fold_lines f (f acc line) chnl
  with End_of_file -> 
	acc

let map_lines_to_list f chnl = 
  List.rev (fold_lines (fun acc line -> (f line) :: acc) [] chnl)

let iter_lines f chnl = fold_lines (fun acc line -> f line) () chnl

let read_lines chnl = map_lines_to_list (fun line -> line) chnl

(*** Output operations ***)

let with_output_file file_name f =
  let chnl = open_out file_name in
  let res = f chnl in
  close_out chnl ;
  res

let write_lines lines chnl = List.iter (output_string chnl) lines

