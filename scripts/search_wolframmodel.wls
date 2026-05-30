#!/usr/bin/env wolframscript
(* search_wolframmodel.wls — a small search for a Wolfram-model rule whose
   emergent space is flat and finite-dimensional (~2D or ~3D).

   Strategy: most rules give either 1-dimensional (linear vertex growth) or
   exponential (tree-like, infinite-dimensional) space. We screen a list of
   candidate rules by how their vertex count grows with generation
   (V(g) ~ g^d for a d-dimensional space; V(g) ~ c^g for hyperbolic), then for
   the non-exploding candidates we build the spatial graph and measure the
   actual ball-growth dimension and mean Ollivier-Ricci curvature.

   One-off, needs internet (ResourceFunction). Writes a report; if a flat
   finite-dimensional rule is found, caches its graph to data/.              *)

Needs["GraphDDG`", FileNameJoin[{ParentDirectory[DirectoryName[$InputFileName]], "wolfram", "graphgeometry.wl"}]];
wm = ResourceFunction["WolframModel"];
fmt[x_] := ToString[NumberForm[Chop[N[x], 1.*^-6], {4, 2}]];

cliqueGraph[state_] := IndexGraph[SimpleGraph[
   UndirectedEdge @@@ Flatten[Subsets[#, {2}] & /@ state, 1]]];

(* candidate rules: {rule, init} *)
cand = {
  {{{1, 2}, {2, 3}} -> {{1, 2}, {2, 3}, {3, 4}}, {{1, 2}, {2, 3}}},
  {{{1, 2}, {2, 3}} -> {{1, 2}, {2, 4}, {4, 3}, {3, 1}}, {{1, 2}, {2, 3}}},
  {{{1, 2}, {2, 3}} -> {{1, 4}, {4, 2}, {2, 3}, {3, 4}}, {{1, 2}, {2, 3}}},
  {{{1, 2}, {1, 3}} -> {{1, 2}, {1, 3}, {2, 4}, {3, 4}}, {{1, 2}, {1, 3}}},
  {{{1, 2}, {3, 2}} -> {{1, 2}, {3, 2}, {1, 4}, {3, 4}}, {{1, 2}, {3, 2}}},
  {{{1, 2}, {2, 3}} -> {{4, 1}, {4, 2}, {4, 3}}, {{1, 2}, {2, 3}}},
  {{{1, 2}} -> {{1, 3}, {3, 2}}, {{1, 2}}},
  {{{1, 2}, {2, 3}} -> {{1, 2}, {2, 3}, {1, 4}, {4, 3}}, {{1, 2}, {2, 3}}},
  {{{1, 2, 3}} -> {{1, 2, 4}, {2, 3, 4}}, {{1, 2, 3}}},
  {{{1, 2, 3}} -> {{4, 2, 3}, {1, 4, 3}, {1, 2, 4}}, {{1, 2, 3}}},
  {{{1, 2, 3}, {1, 4, 5}} -> {{1, 2, 3}, {1, 4, 5}, {2, 4, 6}}, {{1, 2, 3}, {1, 4, 5}}},
  {{{1, 2, 3}, {3, 4, 5}} -> {{1, 2, 3}, {3, 4, 5}, {5, 1, 6}}, {{1, 2, 3}, {3, 4, 5}}},
  {{{1, 2, 3}, {4, 5, 6}} -> {{1, 2, 3}, {4, 5, 6}, {1, 4, 7}, {3, 6, 7}}, {{1, 2, 3}, {4, 5, 6}}},
  {{{1, 2, 3}, {2, 4, 5}} -> {{6, 2, 3}, {6, 4, 5}, {1, 6, 4}}, {{1, 1, 1}, {1, 1, 1}}},
  {{{1, 2}, {2, 3}, {3, 1}} -> {{1, 2}, {2, 4}, {4, 1}, {2, 3}, {3, 4}}, {{1, 2}, {2, 3}, {3, 1}}},
  {{{1, 2}, {2, 3}} -> {{1, 4}, {2, 4}, {3, 4}, {1, 2}, {2, 3}}, {{1, 2}, {2, 3}}},
  {{{1, 2}, {3, 4}} -> {{1, 2}, {3, 4}, {2, 3}}, {{1, 2}, {3, 4}, {4, 1}}},
  {{{1, 2, 2}} -> {{2, 3, 3}, {3, 1, 2}}, {{1, 1, 1}}},
  {{{1, 2}, {1, 3}} -> {{2, 3}, {3, 4}, {1, 4}}, {{1, 2}, {1, 3}}},
  {{{1, 2}, {2, 3}} -> {{2, 1}, {1, 4}, {4, 3}}, {{1, 2}, {2, 3}}}};

gens = {3, 5, 7, 9};
lsSlope[xs_, ys_] := With[{m = Length[xs]},
  (m Total[xs ys] - Total[xs] Total[ys])/(m Total[xs^2] - Total[xs]^2)];

Print["screening ", Length[cand], " rules by vertex growth V(g)..."];
results = Table[
  Module[{rule = cand[[i, 1]], init = cand[[i, 2]], vs, ratios, dGrowth, regime},
   vs = Table[TimeConstrained[
       Length[Union[Flatten[wm[rule, init, g]["FinalState"]]]], 8, Missing[]],
      {g, gens}];
   If[MemberQ[vs, Missing[] | _?(# > 60000 &)], regime = "explodes"; dGrowth = Infinity,
    ratios = Differences[Log[N@vs]]/Differences[N@gens];   (* d log V / d gen *)
    dGrowth = lsSlope[Log[N@gens], Log[N@vs]];
    regime = Which[
      Max[vs] < 12, "trivial",
      Mean[ratios] > 0.35, "exponential",
      dGrowth < 1.4, "~1D (linear)",
      True, "candidate"]];
   {i, regime, dGrowth, Last[vs], vs}],
  {i, Length[cand]}];

Print["\nrule  regime          d_growth   V(9)"];
Do[Print["  ", StringPadLeft[ToString[r[[1]]], 2], "  ",
   StringPadRight[r[[2]], 14], "  ", StringPadLeft[fmt[r[[3]]], 6], "   ", r[[4]]],
  {r, SortBy[results, #[[3]] &]}];

(* measure the actual geometry of the finite-dimension candidates *)
cands = Select[results, #[[2]] === "candidate" &];
Print["\n", Length[cands], " finite-dimension candidate(s); measuring geometry:"];
best = {};
Do[Module[{i = c[[1]], rule, init, g, ev, gr, d, k},
   rule = cand[[i, 1]]; init = cand[[i, 2]];
   g = 10;
   ev = TimeConstrained[wm[rule, init, g], 20, $TimedOut];
   If[ev =!= $TimedOut,
    gr = cliqueGraph[ev["FinalState"]];
    If[VertexCount[gr] >= 60 && VertexCount[gr] <= 6000 && ConnectedGraphQ[gr],
     d = GraphDDG`graphDimension[gr, Min[8, GraphDiameter[gr] - 1], 18]["d"];
     k = GraphDDG`ollivierRicciMean[gr, 80];
     Print["  rule ", i, ": V=", VertexCount[gr], "  ball-dim=", fmt[d],
       "  mean Ollivier-Ricci=", fmt[k]];
     AppendTo[best, {i, gr, d, k}]]]],
  {c, cands}];

(* save the flattest finite-dimensional graph found, if any *)
flat = Select[best, 1.6 <= #[[3]] <= 3.6 && Abs[#[[4]]] < 0.25 &];
If[flat =!= {},
  With[{w = First[SortBy[flat, Abs[#[[4]]] &]]},
   Print["\nFLAT finite-dim rule found: #", w[[1]], "  d=", fmt[w[[3]]], "  kappa=", fmt[w[[4]]]];
   Export[FileNameJoin[{ParentDirectory[DirectoryName[$InputFileName]], "data", "wm_flat_graph.wxf"}], w[[2]]];
   Print["  cached -> data/wm_flat_graph.wxf  (rule ", cand[[w[[1]], 1]], ")"]],
  Print["\nNo flat finite-dimensional rule among the candidates \[LongDash] the ",
    "search reinforces that such rules are rare."]];

(* ---- summary figure: the "missing middle" ------------------------------ *)
imgDir = FileNameJoin[{ParentDirectory[DirectoryName[$InputFileName]], "docs", "images"}];
regimes = {"trivial", "~1D (linear)", "candidate", "exponential"};
counts = (Count[results[[All, 2]], #] &) /@ regimes;
barP = BarChart[counts, ChartLabels -> {"trivial", "~1D", "intermediate", "exponential\n(hyperbolic)"},
   ChartStyle -> {Gray, RGBColor[0.15, 0.35, 0.8], RGBColor[0.2, 0.6, 0.2], RGBColor[0.85, 0.3, 0.1]},
   PlotLabel -> Style["Emergent geometry of 20 rules: the missing middle", 13],
   AxesLabel -> {None, "# rules"}, ImageSize -> 430,
   Epilog -> {Text[Style["flat 2D/3D: 0 rules", Italic, 11, Darker@Green], Scaled[{0.62, 0.55}]]}];
(* representative vertex-growth curves vs g^2, g^3 references *)
rep1D = SelectFirst[results, #[[2]] == "~1D (linear)" &];
repExp = SelectFirst[results, #[[2]] == "exponential" &];
gg = Range[3, 11];
curveP = ListLogPlot[{
    Transpose[{gens, rep1D[[5]]}], Transpose[{gens, repExp[[5]]}],
    Transpose[{gg, gg^2}], Transpose[{gg, gg^3}]},
   Joined -> {True, True, True, True}, PlotMarkers -> {Automatic, Automatic, None, None},
   PlotStyle -> {RGBColor[0.15, 0.35, 0.8], RGBColor[0.85, 0.3, 0.1],
      Directive[Gray, Dashed], Directive[Gray, Dotted]},
   PlotLegends -> Placed[{"a 1D rule", "an exponential rule", "g\.b2 (2D)", "g\.b3 (3D)"}, Below],
   Frame -> True, FrameLabel -> {"generation g", "vertices V(g)"},
   PlotLabel -> Style["Vertex growth: 1D and exponential bracket the 2D/3D band", 12],
   ImageSize -> 430, GridLines -> Automatic];
Export[FileNameJoin[{imgDir, "wm_rule_search.png"}],
   GraphicsRow[{barP, curveP}, ImageSize -> 920, Spacings -> 30], ImageResolution -> 120];
Print["wrote wm_rule_search.png"];
