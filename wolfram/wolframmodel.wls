#!/usr/bin/env wolframscript
(* wolframmodel.wls — the same discrete-geometry tools, pointed at a
   hypergraph from the Wolfram Physics Project.

   The Wolfram model evolves a hypergraph by a rewriting rule; "space" is meant
   to emerge from the resulting combinatorial structure. We generate one such
   hypergraph, then read its geometry off the graph exactly as in
   graphcurvature.wls: ball-growth dimension and Ollivier-Ricci curvature.

   The hypergraph is generated once via ResourceFunction["WolframModel"] and
   the spatial graph cached to data/wm_graph.wxf (a committed asset), so a
   normal rebuild needs no internet.                                          *)

Needs["GraphDDG`", FileNameJoin[{DirectoryName[$InputFileName], "graphgeometry.wl"}]];
repo = ParentDirectory[DirectoryName[$InputFileName]];
imgDir = FileNameJoin[{repo, "docs", "images"}];
graphFile = FileNameJoin[{repo, "data", "wm_graph.wxf"}];
plotFile = FileNameJoin[{imgDir, "wm_hypergraph.png"}];
fmt[x_] := ToString[NumberForm[Chop[N[x], 1.*^-6], {4, 3}]];

rule = {{1, 2, 3}} -> {{1, 2, 4}, {4, 3, 5}, {5, 2, 1}};
init = {{1, 1, 1}}; gens = 5;

cliqueGraph[state_] := Module[{g},
  g = SimpleGraph[UndirectedEdge @@@ Flatten[Subsets[#, {2}] & /@ state, 1]];
  (* largest connected component *)
  First[SortBy[ConnectedGraphComponents[g], -VertexCount[#] &]]];

If[FileExistsQ[graphFile],
  g = Import[graphFile];
  Print["loaded cached spatial graph: V=", VertexCount[g], " E=", EdgeCount[g]],
  (* else generate (needs internet for the resource function) *)
  Print["generating Wolfram model (rule ", rule, ", ", gens, " generations) \[Ellipsis]"];
  wm = ResourceFunction["WolframModel"];
  ev = wm[rule, init, gens];
  g = cliqueGraph[ev["FinalState"]];
  g = Graph[g, VertexList[g] -> Range[VertexCount[g]]];   (* relabel 1..n *)
  Export[graphFile, g];
  Export[plotFile, ev["FinalStatePlot", ImageSize -> 460]];
  Print["  generated + cached: V=", VertexCount[g], " E=", EdgeCount[g]];
];

(* ---- emergent geometry of the hypergraph ------------------------------- *)
res = GraphDDG`graphDimension[g, 7, 25];
Print["ball-growth |B_r| = ", res["counts"]];
Print["  (super-polynomial growth \[Rule] tree-like, no finite dimension)"];
orc = Values[GraphDDG`ollivierRicci[g]];
Print["Ollivier-Ricci curvature: mean = ", fmt[Mean[orc]],
  "   range = ", fmt /@ MinMax[orc]];
Print["  \[RightArrow] negative: this rule grows a HYPERBOLIC emergent space,"];
Print["    in contrast to the flat lattices and the d=2 mesh-graphs."];

(* ---- render the graph coloured by Ollivier-Ricci ----------------------- *)
orcA = GraphDDG`ollivierRicci[g];
inc = Association[Table[v -> {}, {v, VertexList[g]}]];
Do[inc[e[[1]]] = Append[inc[e[[1]]], orcA[e]];
   inc[e[[2]]] = Append[inc[e[[2]]], orcA[e]], {e, Keys[orcA]}];
vorc = Table[Mean[inc[v]], {v, VertexList[g]}];
cmax = Max[Abs[vorc]] + 1.*^-9;
col = AssociationThread[VertexList[g] ->
   Map[ColorData["TemperatureMap"], Rescale[vorc, {-cmax, cmax}]]];
fig = Graph[g, VertexStyle -> Normal[col], VertexSize -> 0.6,
   EdgeStyle -> Directive[GrayLevel[0.7], Opacity[0.4]],
   GraphLayout -> "SpringElectricalEmbedding", ImageSize -> 560,
   PlotLabel -> Style["Wolfram-model spatial graph, coloured by Ollivier-Ricci curvature", 12]];
Export[FileNameJoin[{imgDir, "wm_curvature.png"}], fig, ImageResolution -> 120];
Print["wrote wm_curvature.png"];
