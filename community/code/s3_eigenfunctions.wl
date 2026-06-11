(* The first nine non-constant eigenfunctions on a geodesic sphere. *)
sphere = DDG`icosphereMesh[3];
{vals, vecs} = DDG`laplacianSpectrum[sphere, 17];
spts = DDG`meshCoords[sphere]; stris = DDG`meshTriangles[sphere];

modeFig[phi_, lbl_] := Module[{cmax = Max[Abs[phi]]},
  Graphics3D[{EdgeForm[], GraphicsComplex[spts, Polygon[stris],
      VertexColors -> Map[ColorData["BlueGreenYellow"], Rescale[phi, {-cmax, cmax}]]]},
    Boxed -> False, SphericalRegion -> True, ImageSize -> 260,
    ViewPoint -> {1.5, -2., 1.2}, Lighting -> "Neutral", PlotLabel -> Style[lbl, 11]]];

Grid[Partition[
  Table[modeFig[vecs[[j]],                              (* skip the constant phi0 *)
    "\[Phi]" <> ToString[j - 1] <> "  \[Lambda]=" <> ToString[NumberForm[vals[[j]], {4, 2}]]],
   {j, 2, 10}], 3], Spacings -> {0, 0}]
