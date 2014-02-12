(* Test Suite for Mat3alg *)

open OUnit
open Omath
open Mat3alg

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

let test_tuple_vec _ = 
  assert_equal (0.1, 0.2, 0.3) (tuple_vec (vec 0.1 0.2 0.3))

let test_tuple_covec _ = 
  assert_equal (0.1, 0.2, 0.3) (tuple_covec (covec 0.1 0.2 0.3))

let test_tuple_mat _ =
  assert_equal (0.11, 0.12, 0.13,
				0.21, 0.22, 0.23,
				0.31, 0.32, 0.33)
			   (tuple_mat
				  (mat 0.11 0.12 0.13
					   0.21 0.22 0.23
					   0.31 0.32 0.33))


let suite = "Mat3alg Tests" >::: ["test_vec" >:: test_vec;
								  "test_covec" >:: test_covec;
								  "test_mat" >:: test_mat;
								  "test_tuple_vec" >:: test_tuple_vec;
								  "test_tuple_covec" >:: test_tuple_covec;
								  "test_tuple_mat" >:: test_tuple_mat]
														 

let _ =
  run_test_tt_main suite
