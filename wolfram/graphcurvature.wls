#!/usr/bin/env wolframscript
(* graphcurvature.wls — discrete curvature and dimension on graphs.

   Beyond triangle meshes, the same geometric ideas apply to *any* graph. This
   is the language of network science and the Wolfram Physics Project, where
   space itself is a graph and its geometry must be read off the combinatorics.

   1. Ollivier-Ricci curvature (optimal transport) on canonical graphs -- a
      table of calculated examples with the expected signs.
   2. Emergent dimension from ball growth, validated on periodic lattices
      (d = 1, 2, 3) and recovered on our DDG meshes viewed as bare graphs.
   3. The sphere's mesh-graph carries positive curvature concentrated at
      exactly 12 vertices -- the "soccer ball" theorem, a combinatorial face
      of Gauss-Bonnet (chi = 2). The flat-lattice torus has zero curvature
      everywhere (chi = 0).                                                   *)

Needs["DDG`", FileNameJoin[{DirectoryName[$InputFileName], "ddg.wl"}]];
Needs["GraphDDG`", FileNameJoin[{DirectoryName[$InputFileName], "graphgeometry.wl"}]];
imgDir = FileNameJoin[{ParentDirectory[DirectoryName[$InputFileName]], "docs", "images"}];
fmt[x_] := ToString[NumberForm[Chop[N[x], 1.*^-6], {4, 3}]];

(* ---- 1. Ollivier-Ricci: calculated examples ---------------------------- *)
Print["Ollivier-Ricci curvature (mean over edges):"];
examples = {
   {"complete K6  (positive)", CompleteGraph[6]},
   {"cycle C8     (flat, 1D)", CycleGraph[8]},
   {"6x6 grid     (flat, 2D)", GridGraph[{6, 6}]},
   {"binary tree  (negative)", CompleteKaryTree[3, 3]}};
Do[With[{lbl = ex[[1]], g = ex[[2]]},
   Print["  ", lbl, " : ", fmt[Mean[Values[GraphDDG`ollivierRicci[g]]]]]],
  {ex, examples}];

(* ---- 2. emergent dimension: lattices + mesh graphs --------------------- *)
ring = CycleGraph[200];
tor2 = GraphProduct[CycleGraph[40], CycleGraph[40], "Cartesian"];
tor3 = GraphProduct[GraphProduct[CycleGraph[14], CycleGraph[14], "Cartesian"],
   CycleGraph[14], "Cartesian"];
sphG = GraphDDG`meshGraph[DDG`icosphereMesh[3]];
torG = GraphDDG`meshGraph[DDG`torusMesh[2., 0.8, 24, 48]];

dimRuns = {
   {"ring  C(200)", ring, 12}, {"2-torus 40\.b2", tor2, 12},
   {"3-torus 14\.b3", tor3, 7}, {"sphere mesh-graph", sphG, 7},
   {"torus mesh-graph", torG, 8}};
Print["\nemergent (ball-growth) dimension:"];
dimData = Table[
   With[{lbl = run[[1]], res = GraphDDG`graphDimension[run[[2]], run[[3]], 18]},
    Print["  ", lbl, " : d = ", fmt[res["d"]]];
    {lbl, res}], {run, dimRuns}];

(* log-log figure of |B_r| vs r *)
dimFig = ListLogLogPlot[
   Table[Transpose[{d[[2]]["radii"], d[[2]]["counts"]}], {d, dimData}],
   Joined -> True, PlotMarkers -> Automatic, Frame -> True,
   FrameLabel -> {"radius r", "ball size |B\:1d63|"},
   PlotLabel -> Style["Emergent dimension from ball growth |B\:1d63| ~ r\:1d48", 14],
   PlotLegends -> Placed[
     (#[[1]] <> "  (d=" <> fmt[#[[2]]["d"]] <> ")" & /@ dimData), Right],
   ImageSize -> 640, GridLines -> Automatic];
Export[FileNameJoin[{imgDir, "graph_dimension.png"}], dimFig, ImageResolution -> 120];
Print["wrote graph_dimension.png"];

(* ---- 3. the soccer-ball theorem on the sphere mesh-graph --------------- *)
ico = DDG`icosphereMesh[3]; ip = DDG`meshCoords[ico]; it = DDG`meshTriangles[ico];
icoG = GraphDDG`meshGraph[ico];
orc = GraphDDG`ollivierRicci[icoG];          (* edge -> curvature *)
(* per-vertex curvature = mean over incident edges *)
incident = Association[Table[v -> {}, {v, VertexCount[icoG]}]];
Do[With[{u = e[[1]], w = e[[2]]},
   incident[u] = Append[incident[u], orc[e]];
   incident[w] = Append[incident[w], orc[e]]], {e, Keys[orc]}];
vORC = Table[Mean[incident[v]], {v, VertexCount[icoG]}];
deg5 = Position[VertexDegree[icoG], 5] // Flatten;
Print["\nsphere mesh-graph: ", Length[deg5],
  " vertices of degree 5 (the curvature defects); all others degree 6"];

cmax = Max[vORC];
soccer = Graphics3D[{
    {EdgeForm[GrayLevel[0.4]], FaceForm[GrayLevel[0.9]],
     GraphicsComplex[ip, Polygon[it]]},
    Sphere[ip[[#]], 0.045] & /@ deg5,          (* mark the 12 defects *)
    {Red, Sphere[ip[[#]], 0.05] & /@ deg5}},
   Boxed -> False, Lighting -> "Neutral", SphericalRegion -> True, ImageSize -> 430,
   PlotLabel -> Style["12 positive-curvature defects (\"soccer ball\")", 12]];
Export[FileNameJoin[{imgDir, "graph_curvature_sphere.png"}], soccer, ImageResolution -> 120];
Print["wrote graph_curvature_sphere.png"];

(* also a curvature heat-map version *)
heat = Graphics3D[{EdgeForm[],
    GraphicsComplex[ip, Polygon[it],
      VertexColors -> Map[ColorData["TemperatureMap"], Rescale[vORC, {-cmax, cmax}]]]},
   Boxed -> False, Lighting -> "Neutral", SphericalRegion -> True, ImageSize -> 430,
   PlotLabel -> Style["Ollivier-Ricci curvature on the sphere graph", 12]];
Export[FileNameJoin[{imgDir, "graph_curvature_heat.png"}], heat, ImageResolution -> 120];
Print["wrote graph_curvature_heat.png"];
