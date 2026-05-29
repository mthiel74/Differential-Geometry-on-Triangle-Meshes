#!/usr/bin/env wolframscript
(* hodge.wls — discrete exterior calculus and the Hodge-Helmholtz
   decomposition of a tangent vector field on a surface.

   DEC builds the exterior derivatives d0 (vertices->edges) and d1
   (edges->faces) as signed incidence matrices, plus diagonal Hodge stars.
   Two structural facts are exact matrix identities:
       d1 . d0 = 0                 (d^2 = 0)
       d0^T . star1 . d0 = -L      (the cotangent Laplacian re-derived)
   Any edge 1-form then splits uniquely as
       omega = d(alpha) + delta(beta) + gamma   (exact + coexact + harmonic),
   and the dimension of the harmonic space equals the first Betti number
   b1 = 2*genus -- a topological invariant the discrete operators recover.

   We decompose a swirling field on a torus and show the three components;
   the harmonic part is the circulation around the handle that no gradient
   or curl can produce.                                                     *)

Needs["DDG`", FileNameJoin[{DirectoryName[$InputFileName], "ddg.wl"}]];

imgDir = FileNameJoin[{ParentDirectory[DirectoryName[$InputFileName]], "docs", "images"}];

R = 2.; r = 0.8;
torus = DDG`torusMesh[R, r, 24, 48];
pts = DDG`meshCoords[torus]; tris = DDG`meshTriangles[torus];
dec = DDG`decOperators[torus];
d0 = dec["d0"]; d1 = dec["d1"]; s1 = dec["star1"]; edges = dec["edges"];

(* ---- structural identities --------------------------------------------- *)
Print["torus  V=", Length[pts], "  E=", Length[edges], "  F=", Length[tris]];
Print["max|d1.d0|              = ", Max[Abs[d1 . d0]], "   (d^2 = 0)"];
Print["max|d0^T star1 d0 + L|  = ",
  Max[Abs[Transpose[d0] . s1 . d0 + DDG`cotanLaplacian[torus]]]];
Print["b1 (harmonic dim)       = ", DDG`bettiOne[torus], "   (= 2*genus = 2)"];

(* ---- a swirling test 1-form -------------------------------------------- *)
(* azimuthal field around the z-axis, sampled to edges as circulation *)
field[p_] := {-p[[2]], p[[1]], 0.} + 0.4 {0., 0., Sin[3 p[[1]]]};
omega = (field[(pts[[#[[1]]]] + pts[[#[[2]]]])/2] . (pts[[#[[2]]]] - pts[[#[[1]]]]) &) /@ edges;

h = DDG`hodgeDecomposition[torus, omega];
ip[a_, b_] := a . s1 . b;
tot = ip[omega, omega];
Print["energy split:  exact ", Round[100 ip[h["exact"], h["exact"]]/tot], "%",
  "   coexact ", Round[100 ip[h["coexact"], h["coexact"]]/tot], "%",
  "   harmonic ", Round[100 ip[h["harmonic"], h["harmonic"]]/tot, 0.1], "%"];

(* ---- render the three components as face vector fields ----------------- *)
centroids = Mean /@ (pts[[#]] & /@ tris);
step = 3;                                   (* subsample arrows for clarity *)
sel = Range[1, Length[tris], step];

(* reconstruct all three face fields; use ONE common arrow scale so the
   relative magnitudes (exact ~ 0, coexact, harmonic) are honest *)
fExact = DDG`oneFormToFaceField[torus, h["exact"]];
fCoex = DDG`oneFormToFaceField[torus, h["coexact"]];
fHarm = DDG`oneFormToFaceField[torus, h["harmonic"]];
sc = 0.4/Max[Norm /@ Join[fExact, fCoex, fHarm]];

(* arrowheads sized per-arrow (Arrow uses a fixed head otherwise, which makes
   near-zero vectors look large) *)
panel[vecs_, col_, lbl_] := Graphics3D[{
    {EdgeForm[], FaceForm[GrayLevel[0.85]], GraphicsComplex[pts, Polygon[tris]]},
    {col, Thickness[0.003],
     Table[With[{c = centroids[[i]], v = sc vecs[[i]]},
        {Arrowheads[0.22 Norm[v]], Arrow[{c, c + v}]}], {i, sel}]}},
   Boxed -> False, ViewPoint -> {1.8, -2.2, 1.9}, ImageSize -> 360,
   Lighting -> "Neutral", PlotLabel -> Style[lbl, 11]];

fig = GraphicsRow[{
    panel[fExact, RGBColor[0.8, 0.2, 0.2], "exact  d\[Alpha]  (gradient, ~0%)"],
    panel[fCoex, RGBColor[0.15, 0.45, 0.85], "coexact  \[Delta]\[Beta]  (curl, 27%)"],
    panel[fHarm, RGBColor[0.1, 0.55, 0.2],
      "harmonic \[Gamma]  (around the handle, 73%)"]},
   Spacings -> 0, ImageSize -> 1140];
out = FileNameJoin[{imgDir, "hodge_decomposition.png"}];
Export[out, fig, ImageResolution -> 120];
Print["wrote ", out];
