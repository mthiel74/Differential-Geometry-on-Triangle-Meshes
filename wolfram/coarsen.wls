#!/usr/bin/env wolframscript
(* coarsen.wls — Forman-guided graph coarsening: a curvature renormalization.

   Ricci flow contracts regions of high curvature. We use the cheap Forman-Ricci
   curvature to drive a discrete analogue: repeatedly contract the edge of
   maximal Forman curvature (the most clustered, redundant edge). Two payoffs:

   1. GEOMETRIC RENORMALIZATION. Coarsening a 2D mesh-graph leaves its
      emergent dimension essentially unchanged (~2) over a wide range of
      scales -- dimension is approximately scale-invariant, the hallmark of a
      genuine geometry.
   2. COMMUNITY DETECTION. On a graph with planted communities, the dense
      (high-curvature) intra-community edges collapse first, so coarsening
      merges each community into a super-vertex and exposes the skeleton.     *)

Needs["DDG`", FileNameJoin[{DirectoryName[$InputFileName], "ddg.wl"}]];
Needs["GraphDDG`", FileNameJoin[{DirectoryName[$InputFileName], "graphgeometry.wl"}]];
imgDir = FileNameJoin[{ParentDirectory[DirectoryName[$InputFileName]], "docs", "images"}];
fmt[x_] := ToString[NumberForm[Chop[N[x], 1.*^-6], {3, 2}]];

(* ---- 1. dimension is preserved under coarsening ------------------------ *)
tg = GraphDDG`meshGraph[DDG`torusMesh[2., 0.8, 26, 40]];
fracs = {1., 0.8, 0.65, 0.5, 0.4};
Print["geometric renormalization of a 2D mesh-graph (V0 = ", VertexCount[tg], "):"];
dimSeq = Table[
   Module[{c = If[f == 1., tg, GraphDDG`ricciCoarsen[tg, f]], d},
    d = GraphDDG`graphDimension[c, Min[7, GraphDiameter[c] - 1], 15]["d"];
    Print["  frac ", f, "  V=", VertexCount[c], "  dimension d = ", fmt[d]];
    {VertexCount[c], d}], {f, fracs}];

dimFig = ListLinePlot[dimSeq, PlotMarkers -> Automatic,
   Frame -> True, FrameLabel -> {"vertices after coarsening", "emergent dimension d"},
   PlotLabel -> Style["Dimension is ~scale-invariant under Forman coarsening", 12],
   PlotRange -> {0, 3}, GridLines -> {None, {2}},
   GridLinesStyle -> Directive[Green, Dashed], ImageSize -> 440,
   Epilog -> {Text[Style["d = 2 (a 2D surface)", Italic, 10, Darker@Green], Scaled[{0.55, 0.72}]]}];

(* ---- 2. community detection by coarsening ------------------------------ *)
SeedRandom[3];
k = 4; m = 45; n = k m;                         (* 4 communities of 45 *)
block = Flatten[Table[ConstantArray[b, m], {b, k}]];
pin = 0.32; pout = 0.004;                        (* strongly separated blocks *)
edges = Select[Subsets[Range[n], {2}],
   With[{u = #[[1]], v = #[[2]]},
     RandomReal[] < If[block[[u]] == block[[v]], pin, pout]] &];
comp = First[SortBy[ConnectedGraphComponents[Graph[Range[n], UndirectedEdge @@@ edges]],
    -VertexCount[#] &]];
keepV = VertexList[comp];                        (* original ids in the component *)
bg = IndexGraph[comp];                            (* relabelled 1..|comp| in that order *)
blkC = block[[keepV]];                            (* community labels aligned to bg *)
Print["\ncommunity detection: ", VertexCount[bg], " vertices, ", k, " planted communities"];

{coarse, grp, mem} = GraphDDG`ricciCoarsen[bg, 20./VertexCount[bg], blkC];
(* purity: fraction of vertices whose super-vertex's dominant community = own *)
purity = N@Mean@Flatten@Table[
    Boole[blkC[[#]] == grp[s]] & /@ mem[s], {s, Keys[mem]}];
Print["  coarsened to ", VertexCount[coarse], " super-vertices; purity = ", fmt[purity]];
Print["  super-vertices per community: ", Sort@Tally[Values[grp]]];

cols = {RGBColor[.85,.3,.1], RGBColor[.15,.45,.8], RGBColor[.2,.6,.2], RGBColor[.6,.3,.7]};
origFig = Graph[bg, GraphLayout -> "SpringElectricalEmbedding",
   VertexStyle -> Thread[VertexList[bg] -> cols[[blkC]]], VertexSize -> 1.1,
   EdgeStyle -> Directive[GrayLevel[0.75], Opacity[0.25]], ImageSize -> 360,
   PlotLabel -> Style[ToString[VertexCount[bg]] <> " vertices, 4 planted communities", 11]];
coarseFig = Graph[coarse, GraphLayout -> "SpringElectricalEmbedding",
   VertexStyle -> Normal[cols[[#]] & /@ grp],
   VertexSize -> Normal[(0.3 + 0.04 Length[mem[#]]) & /@ AssociationThread[Keys[mem] -> Keys[mem]]],
   EdgeStyle -> Directive[GrayLevel[0.5], Opacity[0.5]], ImageSize -> 360,
   PlotLabel -> Style["coarsened (size = members): communities recovered", 11]];

Export[FileNameJoin[{imgDir, "coarsen_dimension.png"}], dimFig, ImageResolution -> 120];
Export[FileNameJoin[{imgDir, "coarsen_communities.png"}],
   GraphicsRow[{origFig, coarseFig}, ImageSize -> 760, Spacings -> 20], ImageResolution -> 120];
Print["wrote coarsen_dimension.png + coarsen_communities.png"];
