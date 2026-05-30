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
    ConnectedGraphQ[wg] && Mean[Values[GraphDDG`ollivierRicci[wg]]] < -0.05],
   True];

(* Forman-Ricci: K6 positive, cycle flat, tree negative; and Forman correlates
   positively with Ollivier-Ricci on a heterogeneous graph *)
fK = GraphDDG`formanRicciMean[CompleteGraph[6]];
fCyc = GraphDDG`formanRicciMean[CycleGraph[8]];
fTree = GraphDDG`formanRicciMean[CompleteKaryTree[3, 3]];
SeedRandom[5];
bg = IndexGraph[First[SortBy[ConnectedGraphComponents[
     RandomGraph[BarabasiAlbertGraphDistribution[120, 2]]], -VertexCount[#] &]]];
fA = GraphDDG`formanRicci[bg]; dist = GraphDistanceMatrix[bg];
es = RandomSample[EdgeList[bg], 60];
fo = Correlation[(fA[#] &) /@ es,
   (GraphDDG`ollivierRicci[bg, {#[[1]], #[[2]]}, dist] &) /@ es];
formanOK = fK > 0 && Abs[fCyc] < 0.01 && fTree < 0 && fo > 0.3;

pass = kComplete > 0.3 && Abs[kCycle] < 0.01 && kTree < -0.05 &&
   dRing < 1.2 && 1.4 < dTor2 < 2.2 && nDeg5 == 12 && 1.4 < dSphere < 2.2 && wmOK &&
   formanOK;
Print["Forman: K6=", fK, " cycle=", fCyc, " tree=", fTree, " corr(F,ORC)=", fo];
Print["ORC: K6=", kComplete, "  C8=", kCycle, "  tree=", kTree];
Print["dim: ring=", dRing, "  2-torus=", dTor2, "  sphere graph=", dSphere];
Print["sphere degree-5 defects = ", nDeg5, " (expect 12)"];
Print["Wolfram-model graph hyperbolic = ", wmOK];
Print[If[pass, "PASS", "FAIL"]];
Exit[If[pass, 0, 1]];
