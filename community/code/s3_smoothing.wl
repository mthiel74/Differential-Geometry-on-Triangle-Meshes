(* Spectral mesh smoothing: reconstruct a bumpy sphere from its lowest K
   manifold-harmonic modes (the geometry analogue of a Fourier low-pass). *)
SeedRandom[7];
bumpy = ((1. + 0.13 #[[3]]^2 + 0.10 Sin[3 #[[1]]] Cos[2 #[[2]]]
      + 0.05 RandomReal[{-1, 1}]) Normalize[#]) & /@ spts;
bmesh = DDG`triMesh[bumpy, stris];
{bvals, bvecs} = DDG`laplacianSpectrum[bmesh, 200];   (* 200 lowest modes *)
bM = DDG`massMatrix[bmesh];
coeffs = bvecs . (bM . bumpy);                          (* spectral coefficients (M-inner-product) *)
reconstruct[K_] := Transpose[bvecs[[1 ;; K]]] . coeffs[[1 ;; K]];

renderMesh[x_, lbl_] := Graphics3D[{EdgeForm[],
    GraphicsComplex[x, Polygon[stris],
      VertexColors -> Map[ColorData["BlueGreenYellow"], Rescale[x[[All, 3]]]]]},
   Boxed -> False, SphericalRegion -> True, ImageSize -> 260,
   ViewPoint -> {1.5, -2., 1.2}, Lighting -> "Neutral",
   PlotRange -> 1.3 {{-1, 1}, {-1, 1}, {-1, 1}}, PlotLabel -> Style[lbl, 11]];

Grid[{Append[
   Table[renderMesh[reconstruct[K], "K = " <> ToString[K] <> " modes"], {K, {3, 10, 40, 200}}],
   renderMesh[bumpy, "original"]]}, Spacings -> {0, 0}]
