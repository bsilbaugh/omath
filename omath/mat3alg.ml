(*

 Mat3alg

 Matrix algebra in 3 dimensions.

 Copyright 2014 Benjamin Silbaugh

 *)

(* === Types === *)

(** 3x1 matrix; i.e. vector; i.e. column vector *)
type vec = { vec_1 : float ;
			 vec_2 : float ;
			 vec_3 : float }

(** 1x3 matrix; i.e. covector; i.e. row vector *)
type covec = Covec of vec

(** 3x3 matrix *)
type mat = { mat_11 : float ; mat_12 : float ; mat_13 : float ;
			 mat_21 : float ; mat_22 : float ; mat_23 : float ;
			 mat_31 : float ; mat_32 : float ; mat_33 : float }

(* === Constructors === *)

let vec u1 u2 u3 = { vec_1 = u1 ;
					 vec_2 = u2 ;
					 vec_3 = u3 }

let covec u1 u2 u3 = Covec (vec u1 u2 u3)

let mat e11 e12 e13
		e21 e22 e23
		e31 e32 e33 = { mat_11 = e11 ; mat_12 = e12 ; mat_13 = e13 ;
						mat_21 = e21 ; mat_22 = e22 ; mat_23 = e23 ;
						mat_31 = e31 ; mat_32 = e32 ; mat_33 = e33 }

(* === Accessors === *)

let vec_1 {vec_1; _} = vec_1
let vec_2 {vec_2; _} = vec_2
let vec_3 {vec_3; _} = vec_3

let covec_1 (Covec u) = vec_1 u
let covec_2 (Covec u) = vec_2 u
let covec_3 (Covec u) = vec_3 u

let mat_11 {mat_11; _} = mat_11
let mat_12 {mat_12; _} = mat_12
let mat_13 {mat_13; _} = mat_13
let mat_21 {mat_21; _} = mat_21
let mat_22 {mat_22; _} = mat_22
let mat_23 {mat_23; _} = mat_23
let mat_31 {mat_31; _} = mat_31
let mat_32 {mat_32; _} = mat_32
let mat_33 {mat_33; _} = mat_33

let row_1 {mat_11; mat_12; mat_13; _} = Covec (vec mat_11 mat_12 mat_13)
let row_2 {mat_21; mat_22; mat_23; _} = Covec (vec mat_21 mat_22 mat_23)
let row_3 {mat_31; mat_32; mat_33; _} = Covec (vec mat_31 mat_32 mat_33)

let col_1 {mat_11; mat_21; mat_31; _} = vec mat_11 mat_21 mat_31
let col_2 {mat_21; mat_22; mat_23; _} = vec mat_21 mat_22 mat_23
let col_3 {mat_31; mat_32; mat_33; _} = vec mat_31 mat_32 mat_33

(* === Maps === *)

let vec_map f {vec_1; vec_2; vec_3} = 
  vec (f vec_1) (f vec_2) (f vec_3)

let mat_map f {mat_11 ; mat_12 ; mat_13 ;
			   mat_21 ; mat_22 ; mat_23 ;
			   mat_31 ; mat_32 ; mat_33 } =
  mat (f mat_11) (f mat_12) (f mat_13)
	  (f mat_21) (f mat_22) (f mat_23)
	  (f mat_31) (f mat_32) (f mat_33)

let vec_map2 f u v =
  vec (f u.vec_1 v.vec_1)
	  (f u.vec_2 v.vec_2)
	  (f u.vec_3 v.vec_3)

let mat_map2 f a b =
  mat (f a.mat_11 b.mat_11)
	  (f a.mat_12 b.mat_12)
	  (f a.mat_13 b.mat_13)
	  (f a.mat_21 b.mat_21)
	  (f a.mat_22 b.mat_22)
	  (f a.mat_23 b.mat_23)
	  (f a.mat_31 b.mat_31)
	  (f a.mat_32 b.mat_32)
	  (f a.mat_33 b.mat_33)

(* === Vector +, -, *, / === *)

let ( +:: ) = vec_map2 (+.)

let ( -:: ) = vec_map2 (-.)

let ( *.: ) c = vec_map (fun ui -> c *. ui)

let ( /:. ) u c = (1.0 /. c) *.: u

(* === Covector +, -, *, / === *)

let ( +%% ) (Covec u) (Covec v) = Covec (u +:: v)

let ( -%% ) (Covec u) (Covec v) = Covec (u -:: v)

let ( *.% ) c (Covec u) = Covec (c *.: u)

let ( /%. ) (Covec u) c = Covec (u /:. c)

(* === Matrix +, -, *, / === *)

let ( +@@ ) = mat_map2 (+.)

let ( -@@ ) = mat_map2 (-.)

let ( *.@ ) c = mat_map (fun aij -> c *. aij)

let ( /@. ) a c = (1.0 /. c) *.@ a

(* === Vector - vector  operations === *)

let dot u v = u.vec_1 *. v.vec_1
		      +. u.vec_2 *. v.vec_2
			  +. u.vec_3 *. v.vec_3

let cross u v = vec (u.vec_2 *. v.vec_3 -. u.vec_3 *. v.vec_2)
					(u.vec_3 *. v.vec_1 -. u.vec_1 *. v.vec_3)
					(u.vec_1 *. v.vec_2 -. u.vec_2 *. v.vec_1)

let outer u v = 
  mat (u.vec_1 *. v.vec_1) (u.vec_1 *. v.vec_2) (u.vec_1 *. v.vec_3)
	  (u.vec_2 *. v.vec_1) (u.vec_2 *. v.vec_2) (u.vec_2 *. v.vec_3)
	  (u.vec_3 *. v.vec_1) (u.vec_3 *. v.vec_2) (u.vec_3 *. v.vec_3)

(* === Covector - vector operations === *)

let ( *%: ) (Covec u) v = dot u v

(* === Matrix - Vector operations *)

let ( *@: ) a u = vec ((row_1 a) *%: u) 
					  ((row_2 a) *%: u)
					  ((row_3 a) *%: u)

let ( *%@ ) (Covec u) a = Covec (vec (dot u (col_1 a))
									 (dot u (col_2 a))
									 (dot u (col_3 a)))

let ( *@@ ) a b = let a1 = row_1 a in
				  let a2 = row_2 a in
				  let a3 = row_3 a in
				  let b1 = col_1 b in
				  let b2 = col_2 b in
				  let b3 = col_3 b in
				  mat (a1 *%: b1) (a1 *%: b2) (a1 *%: b3)
					  (a2 *%: b1) (a2 *%: b2) (a2 *%: b3)
					  (a3 *%: b1) (a3 *%: b2) (a3 *%: b3)
