type vec
type covec
type mat

val vec : float -> float -> float -> vec
val covec : float -> float -> float -> covec
val mat :
  float ->
  float -> float -> float -> float -> float -> float -> float -> float -> mat

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


val ( +:: ) : vec -> vec -> vec
val ( -:: ) : vec -> vec -> vec
val ( *.: ) : float -> vec -> vec
val ( /:. ) : vec -> float -> vec

val ( +%% ) : covec -> covec -> covec
val ( -%% ) : covec -> covec -> covec
val ( *.% ) : float -> covec -> covec
val ( /%. ) : covec -> float -> covec

val ( +@@ ) : mat -> mat -> mat
val ( -@@ ) : mat -> mat -> mat
val ( *.@ ) : float -> mat -> mat
val ( /@. ) : mat -> float -> mat

val dot : vec -> vec -> float
val cross : vec -> vec -> vec
val outer : vec -> vec -> mat

val ( *%: ) : covec -> vec -> float
val ( *@: ) : mat -> vec -> vec
val ( *%@ ) : covec -> mat -> covec
val ( *@@ ) : mat -> mat -> mat
