(****

 Base

 Common operators, types, and interfaces used throughout omath

 Copyright 2014 Benjamin Silbaugh

 See LICENSE file for redistribution and modification permissions.

 ****)

(** Unitary composition operator **)
val ( |> ) : 'a -> ( 'a -> 'b ) -> 'b
