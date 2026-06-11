(* The harmonic component as a face vector field: the topological circulation
   around the handle that no gradient or curl can produce (drawn live). *)
fHarm = DDG`oneFormToFaceField[torus, h["harmonic"]];
cen = Mean /@ (pts[[#]] & /@ tris);
sc = 0.4/Max[Norm /@ fHarm];

Graphics3D[{
    {EdgeForm[], FaceForm[GrayLevel[0.85]], GraphicsComplex[pts, Polygon[tris]]},
    {RGBColor[0.1, 0.55, 0.2], Thickness[0.003],
     Table[With[{c = cen[[i]], v = sc fHarm[[i]]},
        {Arrowheads[0.22 Norm[v]], Arrow[{c, c + v}]}], {i, 1, Length[tris], 2}]}},
   Boxed -> False, Lighting -> "Neutral", SphericalRegion -> True, ImageSize -> 440]
