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

let suite = "Mat3alg Tests" >::: ["test_vec" >:: test_vec;
								  "test_covec" >:: test_covec]

let _ =
  run_test_tt_main suite
