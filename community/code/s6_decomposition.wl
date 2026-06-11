(* The full Hodge-Helmholtz split of the swirl into exact / coexact / harmonic,
   all three reconstructed to face fields on one common arrow scale. *)
centroids = Mean /@ (pts[[#]] & /@ tris);
fExact = DDG`oneFormToFaceField[torus, h["exact"]];
fCoex  = DDG`oneFormToFaceField[torus, h["coexact"]];
fHarm  = DDG`oneFormToFaceField[torus, h["harmonic"]];
sc = 0.4/Max[Norm /@ Join[fExact, fCoex, fHarm]];

panel[vecs_, col_, lbl_] := Graphics3D[{
    {EdgeForm[], FaceForm[GrayLevel[0.85]], GraphicsComplex[pts, Polygon[tris]]},
    {col, Thickness[0.003],
     Table[With[{c = centroids[[i]], v = sc vecs[[i]]},
        {Arrowheads[0.22 Norm[v]], Arrow[{c, c + v}]}], {i, 1, Length[tris], 3}]}},
   Boxed -> False, ViewPoint -> {1.8, -2.2, 1.9}, ImageSize -> 360,
   Lighting -> "Neutral", PlotLabel -> Style[lbl, 11]];

GraphicsRow[{
   panel[fExact, RGBColor[0.8, 0.2, 0.2],  "exact d\[Alpha] (gradient, ~0%)"],
   panel[fCoex,  RGBColor[0.15, 0.45, 0.85], "coexact \[Delta]\[Beta] (curl, 27%)"],
   panel[fHarm,  RGBColor[0.1, 0.55, 0.2],  "harmonic \[Gamma] (around the handle, 73%)"]},
  Spacings -> 0]
