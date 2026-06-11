(* Forman-guided coarsening as community detection: dense intra-community edges
   (high curvature) collapse first, merging each planted community to a super-vertex. *)
SeedRandom[3];
k = 4; m = 45; n = k m;                          (* 4 communities of 45 *)
block = Flatten[Table[ConstantArray[b, m], {b, k}]];
edges = Select[Subsets[Range[n], {2}],
   With[{u = #[[1]], v = #[[2]]},
     RandomReal[] < If[block[[u]] == block[[v]], 0.32, 0.004]] &];   (* pin, pout *)
comp = First@SortBy[ConnectedGraphComponents@Graph[Range[n], UndirectedEdge @@@ edges],
    -VertexCount[#] &];
bg = IndexGraph[comp];
blkC = block[[VertexList[comp]]];
{coarse, grp, mem} = GraphDDG`ricciCoarsen[bg, 20./VertexCount[bg], blkC];

cols = {RGBColor[.85, .3, .1], RGBColor[.15, .45, .8], RGBColor[.2, .6, .2], RGBColor[.6, .3, .7]};
GraphicsRow[{
   Graph[bg, GraphLayout -> "SpringElectricalEmbedding",
     VertexStyle -> Thread[VertexList[bg] -> cols[[blkC]]], VertexSize -> 1.1,
     EdgeStyle -> Directive[GrayLevel[0.75], Opacity[0.25]], ImageSize -> 360,
     PlotLabel -> Style[ToString[VertexCount[bg]] <> " vertices, 4 planted communities", 11]],
   Graph[coarse, GraphLayout -> "SpringElectricalEmbedding",
     VertexStyle -> Normal[cols[[#]] & /@ grp],
     VertexSize -> Normal[(0.3 + 0.04 Length[mem[#]]) & /@ AssociationThread[Keys[mem] -> Keys[mem]]],
     EdgeStyle -> Directive[GrayLevel[0.5], Opacity[0.5]], ImageSize -> 360,
     PlotLabel -> Style["coarsened (size = members): communities recovered", 11]]},
  Spacings -> 20]
