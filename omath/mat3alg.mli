
type vec

type covec

type mat 

val vec : float -> float -> float -> vec

val covec : float -> float -> float -> covec

val mat : float -> float -> float -> 
		  float -> float -> float -> 
		  float -> float -> float -> mat

val tuple_vec : vec -> float * float * float

val tuple_covec : covec -> float * float * float

val tuple_mat : mat -> float * float * float *
					   float * float * float *
					   float * float * float
										
val vec_1 : vec -> float
val vec_2 : vec -> float
val vec_3 : vec -> float

val covec_1 : covec -> float
val covec_2 : covec -> float
val covec_3 : covec -> float

val mat_11 : mat -> float
val mat_12 : mat -> float
val mat_13 : mat -> float
val mat_21 : mat -> float
val mat_22 : mat -> float
val mat_23 : mat -> float
val mat_31 : mat -> float
val mat_32 : mat -> float
val mat_33 : mat -> float

val add_vec : vec -> vec -> vec
val sub_vec : vec -> vec -> vec
val mul_num_vec : float -> vec -> vec
val div_vec_num : vec -> float -> vec
val add_covec : covec -> covec -> covec
val sub_covec : covec -> covec -> covec
val mul_num_covec : float -> covec -> covec
val div_covec_num : covec -> float -> covec
val add_mat : mat -> mat -> mat
val sub_mat : mat -> mat -> mat
val mul_num_mat : float -> mat -> mat
val div_mat_num : mat -> float -> mat
val dot : vec -> vec -> float
val cross : vec -> vec -> vec
val outer : vec -> vec -> mat
val mul_covec_vec : covec -> vec -> float
val mul_mat_vec : mat -> vec -> vec
val mul_covec_mat : covec -> mat -> covec
val mul_mat : mat -> mat -> mat
