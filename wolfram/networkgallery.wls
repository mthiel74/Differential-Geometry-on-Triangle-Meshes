#!/usr/bin/env wolframscript
(* networkgallery.wls — a "geometric zoo" of networks.

   The discrete-geometry probes (emergent dimension d from ball growth, mean
   Ollivier-Ricci curvature kappa) are not just for meshes: they CLASSIFY any
   network. We measure (d, kappa) for a small gallery of interesting graphs --
   lattices, a random geometric graph, a small-world and a scale-free network,
   and a Wolfram-model hypergraph -- and place them in the curvature-dimension
   plane. Flat space sits near (kappa = 0, d = integer); small-world and
   hyperbolic networks peel away to negative curvature and high dimension.    *)

Needs["GraphDDG`", FileNameJoin[{DirectoryName[$InputFileName], "graphgeometry.wl"}]];
repo = ParentDirectory[DirectoryName[$InputFileName]];
imgDir = FileNameJoin[{repo, "docs", "images"}];
fmt[x_] := ToString[NumberForm[Chop[N[x], 1.*^-6], {4, 2}]];

(* largest connected component, with vertices relabelled to 1..n
   (IndexGraph does the relabelling; required by ollivierRicci's index lookup) *)
lcc[g_] := IndexGraph[First[SortBy[ConnectedGraphComponents[g], -VertexCount[#] &]]];

SeedRandom[20];
(* lcc relabels vertices to 1..n, required by ollivierRicci's index lookup *)
tor2 = lcc[GraphProduct[CycleGraph[24], CycleGraph[24], "Cartesian"]];
tor3 = lcc[GraphProduct[GraphProduct[CycleGraph[8], CycleGraph[8], "Cartesian"],
   CycleGraph[8], "Cartesian"]];
randGeo = lcc[Quiet@RandomGraph[SpatialGraphDistribution[300, 0.115]]];
smallWorld = lcc[Quiet@RandomGraph[WattsStrogatzGraphDistribution[300, 0.12, 3]]];
scaleFree = lcc[Quiet@RandomGraph[BarabasiAlbertGraphDistribution[300, 2]]];
wmGraph = lcc[Import[FileNameJoin[{repo, "data", "wm_graph.wxf"}]]];

(* {label, graph, rmax-for-dimension} *)
zoo = {
   {"2D lattice (torus)", tor2, 12},
   {"3D lattice (torus)", tor3, 6},
   {"random geometric", randGeo, 7},
   {"small-world (WS)", smallWorld, 4},
   {"scale-free (BA)", scaleFree, 4},
   {"Wolfram model", wmGraph, 6}};

Print["network                 V     E    dim d   Ollivier-Ricci kappa"];
results = Table[
   Module[{lbl = z[[1]], g = z[[2]], rmax = z[[3]], d, k},
    d = GraphDDG`graphDimension[g, Min[rmax, GraphDiameter[g] - 1], 18]["d"];
    k = GraphDDG`ollivierRicciMean[g, 90];
    Print[StringPadRight[lbl, 22], " ", StringPadLeft[ToString[VertexCount[g]], 5],
      " ", StringPadLeft[ToString[EdgeCount[g]], 5], "   ",
      StringPadLeft[fmt[d], 6], "      ", fmt[k]];
    <|"label" -> lbl, "g" -> g, "d" -> d, "k" -> k|>],
   {z, zoo}];

(* ---- gallery of network plots ------------------------------------------ *)
panel[r_] := Graph[r["g"], GraphLayout -> "SpringElectricalEmbedding",
   VertexSize -> 0.5, VertexStyle -> RGBColor[0.2, 0.3, 0.55],
   EdgeStyle -> Directive[GrayLevel[0.65], Opacity[0.35]], ImageSize -> 250,
   PlotLabel -> Style[r["label"] <> "\n d=" <> fmt[r["d"]] <> ", \[Kappa]=" <> fmt[r["k"]], 10]];
gallery = Grid[Partition[panel /@ results, 3], Spacings -> {0.4, 0.4}];
Export[FileNameJoin[{imgDir, "network_gallery.png"}], gallery, ImageResolution -> 110];
Print["wrote network_gallery.png"];

(* ---- the curvature-dimension plane ------------------------------------- *)
pts2 = {#["d"], #["k"]} & /@ results;
labels = #["label"] & /@ results;
scatter = ListPlot[
   MapThread[Labeled[#1, Style[#2, 9]] &, {pts2, labels}],
   Frame -> True, FrameLabel -> {"emergent dimension  d", "mean Ollivier-Ricci  \[Kappa]"},
   PlotLabel -> Style["The curvature-dimension plane: a geometric classification", 13],
   PlotStyle -> Directive[PointSize[0.025], RGBColor[0.8, 0.25, 0.1]],
   GridLines -> {None, {0}}, GridLinesStyle -> Directive[Gray, Dashed],
   PlotRange -> All, ImageSize -> 620, ImagePadding -> {{60, 90}, {50, 40}}];
Export[FileNameJoin[{imgDir, "network_plane.png"}], scatter, ImageResolution -> 120];
Print["wrote network_plane.png"];
