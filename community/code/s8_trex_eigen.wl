(* Laplace-Beltrami eigenfunctions (manifold harmonics) on the coarse scan. *)
coarse = Import["data/trex_coarse.obj", "MeshRegion"];
cpts = DDG`meshCoords[coarse]; ctris = DDG`meshTriangles[coarse];
{vals, vecs} = DDG`laplacianSpectrum[coarse, 7];

modeFig[phi_, lbl_] := Module[{cmax = Max[Abs[phi]]},
  Graphics3D[{EdgeForm[], GraphicsComplex[cpts, Polygon[ctris],
      VertexColors -> Map[ColorData["BlueGreenYellow"], Rescale[phi, {-cmax, cmax}]]]},
    Boxed -> False, ViewPoint -> {0.55, -2.25, 0.05}, ViewVertical -> {1, 0, 0},
    ImageSize -> 250, Lighting -> "Neutral", PlotLabel -> Style[lbl, 11]]];

Grid[Partition[
  Table[modeFig[vecs[[j]],
    "\[Phi]" <> ToString[j - 1] <> "  \[Lambda]=" <> ToString[NumberForm[vals[[j]], {4, 2}]]],
   {j, 2, 7}], 3], Spacings -> {0, 0}]
