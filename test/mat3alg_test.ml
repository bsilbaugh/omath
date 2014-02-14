(* Test Suite for Mat3alg *)

open OUnit
open Omath
open Mat3alg

let assert_close eps x y = assert_equal true (abs_float (x -. y) < eps)

let assert_vec_equal u v =
  assert_equal (vec_1 u) (vec_1 v);
  assert_equal (vec_2 u) (vec_2 v);
  assert_equal (vec_3 u) (vec_3 v)

let assert_vec_close eps u v =
  assert_close eps (vec_1 u) (vec_1 v);
  assert_close eps (vec_2 u) (vec_2 v);
  assert_close eps (vec_3 u) (vec_3 v)

let assert_abstract_vec_close elem_1 elem_2 elem_3 eps u v =
  assert_close eps (elem_1 u) (elem_1 v);
  assert_close eps (elem_2 u) (elem_2 v);
  assert_close eps (elem_3 u) (elem_3 v)

let assert_vec_close = assert_abstract_vec_close vec_1 vec_2 vec_3

let assert_covec_close = assert_abstract_vec_close covec_1 covec_2 covec_3

let assert_mat_close eps a b =
  let check elem_ij p q = 
	assert_close eps (elem_ij p) (elem_ij q) 
  in
  check mat_11 a b; check mat_12 a b; check mat_13 a b;
  check mat_21 a b; check mat_22 a b; check mat_23 a b;
  check mat_31 a b; check mat_32 a b; check mat_33 a b

(* === CONSTRUCTORS AND ACCESSORS === *)

let test_vec _ =
  assert_equal 1.0 (vec_1 (vec 1.0 2.0 3.0));
  assert_equal 2.0 (vec_2 (vec 1.0 2.0 3.0));
  assert_equal 3.0 (vec_3 (vec 1.0 2.0 3.0))

let test_covec _ = 
  assert_equal 1.0 (covec_1 (covec 1.0 2.0 3.0));
  assert_equal 2.0 (covec_2 (covec 1.0 2.0 3.0));
  assert_equal 3.0 (covec_3 (covec 1.0 2.0 3.0))

let test_mat _ =
  let a = mat 0.11 0.12 0.13
			  0.21 0.22 0.23
			  0.31 0.32 0.33 in
  assert_equal 0.11 (mat_11 a);
  assert_equal 0.12 (mat_12 a);
  assert_equal 0.13 (mat_13 a);
  assert_equal 0.21 (mat_21 a);
  assert_equal 0.22 (mat_22 a);
  assert_equal 0.23 (mat_23 a);
  assert_equal 0.31 (mat_31 a);
  assert_equal 0.32 (mat_32 a);
  assert_equal 0.33 (mat_33 a)

(* === Vector algebraic operators === *)

let test_vec_alg _ =
  let check = assert_vec_close 0.5E-12 in
  check (vec 1.1 2.2 3.3) 
		((vec 1.0 2.0 3.0) +:: (vec 0.1 0.2 0.3));
  check (vec 0.9 1.8 2.7)
		((vec 1.0 2.0 3.0) -:: (vec 0.1 0.2 0.3));
  check (vec 0.1 0.2 0.3) 
		(0.1 *.: (vec 1.0 2.0 3.0));
  check (vec 0.1 0.2 0.3)
		((vec 1.0 2.0 3.0) /:. 10.0)

let test_covec_alg _ =
  let check = assert_covec_close 0.5E-12 in
  check (covec 1.1 2.2 3.3) 
		((covec 1.0 2.0 3.0) +%% (covec 0.1 0.2 0.3));
  check (covec 0.9 1.8 2.7)
		((covec 1.0 2.0 3.0) -%% (covec 0.1 0.2 0.3));
  check (covec 0.1 0.2 0.3) 
		(0.1 *.% (covec 1.0 2.0 3.0));
  check (covec 0.1 0.2 0.3)
		((covec 1.0 2.0 3.0) /%. 10.0)

let test_mat_alg _ =
  let check = assert_mat_close 0.5E-12 in
  check (mat 11.11 12.12 13.13
			 21.21 22.22 23.23
			 31.31 32.32 33.33)
		((mat 0.11 0.12 0.13
			  0.21 0.22 0.23
			  0.31 0.32 0.33) +@@
		   (mat 11.0 12.0 13.0
				21.0 22.0 23.0
				31.0 32.0 33.0));
  check (mat 11.11 12.12 13.13
			 21.21 22.22 23.23
			 31.31 32.32 33.33)
		((mat 11.0 12.0 13.0
			  21.0 22.0 23.0
			  31.0 32.0 33.0) -@@
		   (mat (-0.11) (-0.12) (-0.13)
				(-0.21) (-0.22) (-0.23)
				(-0.31) (-0.32) (-0.33)));
  check (mat 1.1 1.2 1.3
			 2.1 2.2 2.3
			 3.1 3.2 3.3)
		(0.1 *.@ (mat 11.0 12.0 13.0
					  21.0 22.0 23.0
					  31.0 32.0 33.0));
  check (mat 1.1 1.2 1.3
			 2.1 2.2 2.3
			 3.1 3.2 3.3)
		((mat 11.0 12.0 13.0
			  21.0 22.0 23.0
			  31.0 32.0 33.0) /@. 10.0)

let test_dot _ =
  let check = assert_close 0.5E-12 in
  check 0.0 (dot (vec 0.1 0.2 0.3) (vec 0.0 0.0 0.0));
  check 1.0 (dot (vec 1.0 0.0 0.0) (vec 1.0 0.0 0.0));
  check 0.0 (dot (vec 1.0 0.0 0.0) (vec 0.0 1.0 0.0));
  check 0.0 (dot (vec 1.0 0.0 0.0) (vec 0.0 0.0 1.0))

let test_cross _ =
  let check = assert_vec_close 0.5E-12 in
  let zero = vec 0.0 0.0 0.0 in
  let i = vec 1.0 0.0 0.0 in
  let j = vec 0.0 1.0 0.0 in
  let k = vec 0.0 0.0 1.0 in
  check zero (cross i i);
  check k (cross i j);
  check (vec 0.0 (-1.0) 0.0) (cross i k);
  check (vec 0.0 0.0 (-1.0)) (cross j i);
  check zero (cross j j);
  check i (cross j k);
  check j (cross k i);
  check (vec (-1.0) 0.0 0.0) (cross k j);
  check zero (cross k k)

let test_outer _ =
  let check = assert_mat_close 0.5E-12 in
  check (mat 0.0 0.0 0.0
			 0.0 0.0 0.0
			 0.0 0.0 0.0) (outer (vec 1.0 0.0 0.0)
								 (vec 0.0 0.0 0.0));
  check (mat 1.0 0.0 0.0
			 0.0 0.0 0.0
			 0.0 0.0 0.0) (outer (vec 1.0 0.0 0.0)
								 (vec 1.0 0.0 0.0));
  check (mat 0.0 1.0 0.0
			 0.0 0.0 0.0
			 0.0 0.0 0.0) (outer (vec 1.0 0.0 0.0)
								 (vec 0.0 1.0 0.0));
  check (mat 0.0 0.0 1.0
			 0.0 0.0 0.0
			 0.0 0.0 0.0) (outer (vec 1.0 0.0 0.0)
								 (vec 0.0 0.0 1.0));
  check (mat 0.0 0.0 0.0
			 1.0 0.0 0.0
			 0.0 0.0 0.0) (outer (vec 0.0 1.0 0.0)
								 (vec 1.0 0.0 0.0));
  check (mat 0.0 0.0 0.0
			 0.0 1.0 0.0
			 0.0 0.0 0.0) (outer (vec 0.0 1.0 0.0)
								 (vec 0.0 1.0 0.0));
  check (mat 0.0 0.0 0.0
			 0.0 0.0 1.0
			 0.0 0.0 0.0) (outer (vec 0.0 1.0 0.0)
								 (vec 0.0 0.0 1.0));
  check (mat 0.0 0.0 0.0
			 0.0 0.0 0.0
			 1.0 0.0 0.0) (outer (vec 0.0 0.0 1.0)
								 (vec 1.0 0.0 0.0));
  check (mat 0.0 0.0 0.0
			 0.0 0.0 0.0
			 0.0 1.0 0.0) (outer (vec 0.0 0.0 1.0)
								 (vec 0.0 1.0 0.0));
  check (mat 0.0 0.0 0.0
			 0.0 0.0 0.0
			 0.0 0.0 1.0) (outer (vec 0.0 0.0 1.0)
								 (vec 0.0 0.0 1.0))

let test_covec_vec_product _ =
  let check = assert_close 0.5E-12 in
  check 0.0 ((covec 1.0 2.0 3.0) *%: (vec 0.0 0.0 0.0));
  check 0.0 ((covec 0.0 0.0 0.0) *%: (vec 1.0 2.0 3.0));
  check 0.0 ((covec 1.0 0.0 0.0) *%: (vec 0.0 0.0 0.0));
  check 1.0 ((covec 1.0 0.0 0.0) *%: (vec 1.0 0.0 0.0));
  check 0.0 ((covec 1.0 0.0 0.0) *%: (vec 0.0 1.0 0.0));
  check 0.0 ((covec 1.0 0.0 0.0) *%: (vec 0.0 0.0 1.0));
  check 0.0 ((covec 0.0 1.0 0.0) *%: (vec 0.0 0.0 0.0));
  check 0.0 ((covec 0.0 1.0 0.0) *%: (vec 1.0 0.0 0.0));
  check 1.0 ((covec 0.0 1.0 0.0) *%: (vec 0.0 1.0 0.0));
  check 0.0 ((covec 0.0 1.0 0.0) *%: (vec 0.0 0.0 1.0));
  check 0.0 ((covec 0.0 0.0 1.0) *%: (vec 0.0 0.0 0.0));
  check 0.0 ((covec 0.0 0.0 1.0) *%: (vec 1.0 0.0 0.0));
  check 0.0 ((covec 0.0 0.0 1.0) *%: (vec 0.0 1.0 0.0));
  check 1.0 ((covec 0.0 0.0 1.0) *%: (vec 0.0 0.0 1.0))

let test_mul_mat_vec _ =
  let check = assert_vec_close 0.5E-12 in
  check (vec 1.1 2.1 3.1) 
		((mat 1.1 1.2 1.3
			  2.1 2.2 2.3
			  3.1 3.2 3.3) *@: (vec 1.0 0.0 0.0));
  check (vec 1.2 2.2 3.2) 
		((mat 1.1 1.2 1.3
			  2.1 2.2 2.3
			  3.1 3.2 3.3) *@: (vec 0.0 1.0 0.0));
  check (vec 1.3 2.3 3.3) 
		((mat 1.1 1.2 1.3
			  2.1 2.2 2.3
			  3.1 3.2 3.3) *@: (vec 0.0 0.0 1.0))

let test_mul_covec_mat _ =
  let check = assert_covec_close 0.5E-12 in
  check (covec 1.1 1.2 1.3)
		((covec 1.0 0.0 0.0) *%@ (mat 1.1 1.2 1.3
									  2.1 2.2 2.3
									  3.1 3.2 3.3));
  check (covec 2.1 2.2 2.3)
		((covec 0.0 1.0 0.0) *%@ (mat 1.1 1.2 1.3
									  2.1 2.2 2.3
									  3.1 3.2 3.3));
  check (covec 3.1 3.2 3.3)
		((covec 0.0 0.0 1.0) *%@ (mat 1.1 1.2 1.3
									  2.1 2.2 2.3
									  3.1 3.2 3.3))

let test_mul_mat_mat _ =
  let check = assert_mat_close 0.5E-12 in
  check (mat 1.1 0.0 0.0
			 2.1 0.0 0.0
			 3.1 0.0 0.0)
		((mat 1.1 1.2 1.3
			  2.1 2.2 2.3
			  3.1 3.2 3.3) *@@ (mat 1.0 0.0 0.0
									0.0 0.0 0.0
									0.0 0.0 0.0));
  check (mat 0.0 1.1 0.0
			 0.0 2.1 0.0
			 0.0 3.1 0.0)
		((mat 1.1 1.2 1.3
			  2.1 2.2 2.3
			  3.1 3.2 3.3) *@@ (mat 0.0 1.0 0.0
									0.0 0.0 0.0
									0.0 0.0 0.0));
  check (mat 0.0 0.0 1.1
			 0.0 0.0 2.1
			 0.0 0.0 3.1)
		((mat 1.1 1.2 1.3
			  2.1 2.2 2.3
			  3.1 3.2 3.3) *@@ (mat 0.0 0.0 1.0
									0.0 0.0 0.0
									0.0 0.0 0.0));
  check (mat 1.2 0.0 0.0
			 2.2 0.0 0.0
			 3.2 0.0 0.0)
		((mat 1.1 1.2 1.3
			  2.1 2.2 2.3
			  3.1 3.2 3.3) *@@ (mat 0.0 0.0 0.0
									1.0 0.0 0.0
									0.0 0.0 0.0));
  check (mat 0.0 1.2 0.0
			 0.0 2.2 0.0
			 0.0 3.2 0.0)
		((mat 1.1 1.2 1.3
			  2.1 2.2 2.3
			  3.1 3.2 3.3) *@@ (mat 0.0 0.0 0.0
									0.0 1.0 0.0
									0.0 0.0 0.0));
  check (mat 0.0 0.0 1.2
			 0.0 0.0 2.2
			 0.0 0.0 3.2)
		((mat 1.1 1.2 1.3
			  2.1 2.2 2.3
			  3.1 3.2 3.3) *@@ (mat 0.0 0.0 0.0
									0.0 0.0 1.0
									0.0 0.0 0.0));
  check (mat 1.3 0.0 0.0
			 2.3 0.0 0.0
			 3.3 0.0 0.0)
		((mat 1.1 1.2 1.3
			  2.1 2.2 2.3
			  3.1 3.2 3.3) *@@ (mat 0.0 0.0 0.0
									0.0 0.0 0.0
									1.0 0.0 0.0));
  check (mat 0.0 1.3 0.0
			 0.0 2.3 0.0
			 0.0 3.3 0.0)
		((mat 1.1 1.2 1.3
			  2.1 2.2 2.3
			  3.1 3.2 3.3) *@@ (mat 0.0 0.0 0.0
									0.0 0.0 0.0
									0.0 1.0 0.0));
  check (mat 0.0 0.0 1.3
			 0.0 0.0 2.3
			 0.0 0.0 3.3)
		((mat 1.1 1.2 1.3
			  2.1 2.2 2.3
			  3.1 3.2 3.3) *@@ (mat 0.0 0.0 0.0
									0.0 0.0 0.0
									0.0 0.0 1.0));
  check (mat 0.0 0.0 0.0
			 0.0 0.0 0.0
			 0.0 0.0 0.0)
		((mat 1.1 1.2 1.3
			  2.1 2.2 2.3
			  3.1 3.2 3.3) *@@ (mat 0.0 0.0 0.0
									0.0 0.0 0.0
									0.0 0.0 0.0))


let suite = "Mat3alg Tests" >::: 
			  ["Vector constructors and accessors" >:: test_vec;
			   "Covector constructors and accessors" >:: test_covec;
			   "Matrix constructors and accessors" >:: test_mat;
			   "Vector +, -, *, /" >:: test_vec_alg;
			   "Covector +, -, *, /" >:: test_covec_alg;
			   "Matrix +, -, *, /" >:: test_mat_alg;
			   "Dot product" >:: test_dot;
			   "Cross product" >:: test_cross;
			   "Outer product" >:: test_outer;
			   "Covector vector product" >:: test_covec_vec_product;
			   "Matrix vector product" >:: test_mul_mat_vec;
			   "Covector matrix product" >:: test_mul_covec_mat;
			   "Matrix matrix product" >:: test_mul_mat_mat]

let _ =
  run_test_tt_main suite
