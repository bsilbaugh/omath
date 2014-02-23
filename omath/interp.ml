(****

  Interp

  Copyright 2014 Benjamin Silbaugh

  See LICENSE file for redistribution and modification permissions.

  Grids are assumed to be an ordered sequence of floating point numbers,
  and field values are a vectors of some kind.

 ****)

(*** Module Parameters ***)

let ( +&& ) = ( +. )
let ( -&& ) = ( -. )
let ( *.& ) = ( *. )
let ( /&. ) = ( /. )

let grid_size arr = Array.length arr

let grid_ref arr i = arr.(i)

let grid_set arr i x = arr.(i) <- x

let vals_size arr = Array.length arr

let vals_ref arr i = arr.(i)

let vals_set arr i x = arr.(i) <- x

(*** Misc Utilities ***)

(** Sets the k element of array arr to value v, and then returns arr **)
let setret arr k v = Array.set arr k v; arr

(** Partitions the grid y relative to the grid x
 
 The grids x and y are presumed to correspond to ordered sequences; i.e.
 x(0) < ... < x(n) and y(0) < ... < y(m), respectively. Furthermore,
 is assumed that x(0) <= y(j) < x(n) for any 0 <= j <= m.

 Given the foregoing conditions, a third sequence {i(0), ..., i(m)} is
 constructed such that, for any 0 <= j <= m, 
 x(i(j)) <= y(j) < x(i(j) + 1).

 The sequence i is returned in the form of a standard integer array.

 EXAMPLE: Suppose x = [| -0.5; 0.0; 0.2; 1.1; 2.0 |] and 
 y = [| -0.1; 0.0; 0.1; 0.19; 1.5; 1.9 |]. Then evalution of

      let i = partition x y

  would yield

      i = [| 0; 1; 1; 1; 3; 3 |]

 **)
let partition x y =
  let n = grid_size x in
  let m = grid_size y in
  let rec ifill i j ihint = 
	if (j < m) && (ihint < (n-1)) then
	  if (grid_ref y j) < (grid_ref x (ihint + 1)) then
		ifill (setret i j ihint) (j + 1) ihint
	  else
		ifill i j (ihint + 1)
	else
	  i
  in ifill (Array.make m 0) 0 0
  
  
(*** Interpolation Procedures ***)

let pw_linear src dst =
  let i = partition src dst in
  let sfun j xj =
	let ij = i.(j) in
	let a = (grid_ref src ij) in
	let b = (grid_ref src (ij + 1)) in
	(xj -. a) /. (b -. a)
  in
  let s = Array.mapi sfun dst in
  fun src_vals ->
  let eval_at j sj =
	let ij = i.(j) in
	let fa = (vals_ref src_vals ij) in
	let fb = (vals_ref src_vals (ij + 1)) in
	((1.0 -. sj) *.& fa) +&& (sj *.& fb)
  in Array.mapi eval_at s
    
(*** Extrapolation Procedures ***)

(*** Composite Interpolation/Extrapolation Procedures ***)
