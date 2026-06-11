(* Discrete exterior calculus on the torus: exterior derivatives d0, d1 and
   diagonal Hodge stars, the two structural identities, and the first Betti
   number. Then a swirling 1-form omega to decompose. *)
R = 2.; r = 0.8;
torus = DDG`torusMesh[R, r, 24, 48];
pts = DDG`meshCoords[torus]; tris = DDG`meshTriangles[torus];
dec = DDG`decOperators[torus];
d0 = dec["d0"]; d1 = dec["d1"]; s1 = dec["star1"]; edges = dec["edges"];

Max[Abs[d1 . d0]]                                          (* 0 : d^2 = 0 *)
Max[Abs[Transpose[d0] . s1 . d0 + DDG`cotanLaplacian[torus]]]  (* 0 : L = d0^T star1 d0 *)
DDG`bettiOne[torus]                                        (* 2 = 2*genus *)

field[p_] := {-p[[2]], p[[1]], 0.} + 0.4 {0., 0., Sin[3 p[[1]]]};
omega = (field[(pts[[#[[1]]]] + pts[[#[[2]]]])/2] . (pts[[#[[2]]]] - pts[[#[[1]]]]) &) /@ edges;
h = DDG`hodgeDecomposition[torus, omega];                  (* exact + coexact + harmonic *)
