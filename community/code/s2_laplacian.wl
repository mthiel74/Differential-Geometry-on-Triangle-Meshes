(* The cotangent Laplacian and its structural properties, plus the mass matrix. *)
mesh = DDG`icosphereMesh[5];
L = DDG`cotanLaplacian[mesh];                  (* V x V SparseArray *)
Max[Abs[L - Transpose[L]]]                     (* 0       : symmetric *)
Max[Abs[L . ConstantArray[1., Length[L]]]]     (* ~1e-15  : constants in the kernel, L.1 = 0 *)
M = DDG`massMatrix[mesh];                       (* diagonal lumped vertex areas *)
