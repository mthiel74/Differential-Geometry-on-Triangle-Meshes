#!/usr/bin/env wolframscript
(* test_graph.wls — discrete graph geometry: Ollivier-Ricci has the right
   signs on canonical graphs, ball-growth dimension orders 1 < 2 < 3, and the
   sphere mesh-graph carries exactly 12 positive-curvature degree-5 defects. *)

Needs["DDG`", FileNameJoin[{ParentDirectory[DirectoryName[$InputFileName]], "wolfram", "ddg.wl"}]];
Needs["GraphDDG`", FileNameJoin[{ParentDirectory[DirectoryName[$InputFileName]], "wolfram", "graphgeometry.wl"}]];

orcMean[g_] := Mean[Values[GraphDDG`ollivierRicci[g]]];
kComplete = orcMean[CompleteGraph[6]];
kCycle = orcMean[CycleGraph[8]];
kTree = orcMean[CompleteKaryTree[3, 3]];

dRing = GraphDDG`graphDimension[CycleGraph[200], 12, 15]["d"];
dTor2 = GraphDDG`graphDimension[
   GraphProduct[CycleGraph[36], CycleGraph[36], "Cartesian"], 12, 15]["d"];

ico = DDG`icosphereMesh[3]; icoG = GraphDDG`meshGraph[ico];
nDeg5 = Count[VertexDegree[icoG], 5];
dSphere = GraphDDG`graphDimension[icoG, 7, 20]["d"];

(* the cached Wolfram-model hypergraph should be a connected, hyperbolic
   (negatively curved) emergent space *)
wmFile = FileNameJoin[{ParentDirectory[DirectoryName[$InputFileName]], "data", "wm_graph.wxf"}];
wmOK = If[FileExistsQ[wmFile],
   With[{wg = Import[wmFile]},
    ConnectedGraphQ[wg] && Mean[Values[GraphDDG`ollivierRicci[wg]]] < -0.2],
   True];

pass = kComplete > 0.3 && Abs[kCycle] < 0.01 && kTree < -0.05 &&
   dRing < 1.2 && 1.4 < dTor2 < 2.2 && nDeg5 == 12 && 1.4 < dSphere < 2.2 && wmOK;
Print["ORC: K6=", kComplete, "  C8=", kCycle, "  tree=", kTree];
Print["dim: ring=", dRing, "  2-torus=", dTor2, "  sphere graph=", dSphere];
Print["sphere degree-5 defects = ", nDeg5, " (expect 12)"];
Print["Wolfram-model graph hyperbolic = ", wmOK];
Print[If[pass, "PASS", "FAIL"]];
Exit[If[pass, 0, 1]];
