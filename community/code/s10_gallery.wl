(* A geometric zoo of networks, each measured for (d, kappa) and laid out. *)
lcc[g_] := IndexGraph[First@SortBy[ConnectedGraphComponents[g], -VertexCount[#] &]];
SeedRandom[20];
zoo = {
   {"2D lattice (torus)", lcc@GraphProduct[CycleGraph[24], CycleGraph[24], "Cartesian"], 12},
   {"3D lattice (torus)", lcc@GraphProduct[GraphProduct[CycleGraph[8], CycleGraph[8], "Cartesian"],
       CycleGraph[8], "Cartesian"], 6},
   {"random geometric", lcc@RandomGraph[SpatialGraphDistribution[300, 0.115]], 7},
   {"small-world (WS)", lcc@RandomGraph[WattsStrogatzGraphDistribution[300, 0.12, 3]], 4},
   {"scale-free (BA)", lcc@RandomGraph[BarabasiAlbertGraphDistribution[300, 2]], 4},
   {"Wolfram model", lcc@Import["data/wm_graph.wxf"], 6}};

results = Table[
   Module[{g = z[[2]]},
    <|"label" -> z[[1]], "g" -> g,
      "d" -> GraphDDG`graphDimension[g, Min[z[[3]], GraphDiameter[g] - 1], 18]["d"],
      "k" -> GraphDDG`ollivierRicciMean[g, 90]|>], {z, zoo}];

Grid[Partition[
   Graph[#["g"], GraphLayout -> "SpringElectricalEmbedding", VertexSize -> 0.5,
      VertexStyle -> RGBColor[0.2, 0.3, 0.55],
      EdgeStyle -> Directive[GrayLevel[0.65], Opacity[0.35]], ImageSize -> 250,
      PlotLabel -> Style[#["label"] <> "\n d=" <> ToString[NumberForm[#["d"], {3, 2}]] <>
         ", \[Kappa]=" <> ToString[NumberForm[#["k"], {3, 2}]], 10]] & /@ results, 3],
   Spacings -> {0.4, 0.4}]
