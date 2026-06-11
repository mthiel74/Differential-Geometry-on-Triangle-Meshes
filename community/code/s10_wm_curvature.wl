(* Read the hypergraph's geometry with the same tools: super-polynomial ball
   growth (no finite dimension) and negative Ollivier-Ricci, coloured per vertex. *)
g = IndexGraph[Import["data/wm_graph.wxf"]];        (* cached spatial graph *)
GraphDDG`graphDimension[g, 7, 25]["counts"]          (* super-polynomial -> tree-like *)

orcA = GraphDDG`ollivierRicci[g];
inc = Association[Table[v -> {}, {v, VertexList[g]}]];
Do[inc[e[[1]]] = Append[inc[e[[1]]], orcA[e]];
   inc[e[[2]]] = Append[inc[e[[2]]], orcA[e]], {e, Keys[orcA]}];
vorc = Table[Mean[inc[v]], {v, VertexList[g]}];
cmax = Max[Abs[vorc]] + 1.*^-9;

Graph[g, VertexStyle -> Normal@AssociationThread[VertexList[g] ->
     Map[ColorData["TemperatureMap"], Rescale[vorc, {-cmax, cmax}]]],
   VertexSize -> 0.6, EdgeStyle -> Directive[GrayLevel[0.7], Opacity[0.4]],
   GraphLayout -> "SpringElectricalEmbedding", ImageSize -> 560]
