(* Forman-Ricci (a cheap, purely local count) vs Ollivier-Ricci, edge by edge,
   on two contrasting networks. Strong correlation = a good fast proxy. *)
SeedRandom[7];
compare[g0_, nEdges_] := Module[{g = IndexGraph[g0], fA, dist, es},
  fA = GraphDDG`formanRicci[g];
  dist = GraphDistanceMatrix[g];
  es = RandomSample[EdgeList[g], Min[nEdges, EdgeCount[g]]];
  {fA[#], GraphDDG`ollivierRicci[g, {#[[1]], #[[2]]}, dist]} & /@ es];

scaleFree = First@SortBy[ConnectedGraphComponents@
     RandomGraph[BarabasiAlbertGraphDistribution[260, 2]], -VertexCount[#] &];
randGeo = First@SortBy[ConnectedGraphComponents@
     RandomGraph[SpatialGraphDistribution[260, 0.13]], -VertexCount[#] &];
dSF = compare[scaleFree, 150];
dRG = compare[randGeo, 150];

panel[data_, lbl_, col_] := ListPlot[data[[All, {2, 1}]],
   Frame -> True, FrameLabel -> {"Ollivier-Ricci \[Kappa]", "Forman-Ricci F"},
   PlotStyle -> Directive[col, PointSize[0.02], Opacity[0.6]],
   PlotLabel -> Style[lbl <> "  (r = " <>
      ToString[NumberForm[Correlation[data[[All, 1]], data[[All, 2]]], {3, 2}]] <> ")", 12],
   GridLines -> {{0}, {0}}, GridLinesStyle -> Directive[Gray, Dashed], ImageSize -> 400];

GraphicsRow[{panel[dSF, "scale-free (hubs)", RGBColor[0.85, 0.3, 0.1]],
   panel[dRG, "random geometric (clustered)", RGBColor[0.15, 0.35, 0.8]]}, Spacings -> 20]
