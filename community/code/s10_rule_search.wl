(* A first 20-rule screen for a flat, finite-dimensional emergent space.
   One-off, needs internet (ResourceFunction). Full driver lives in
   scripts/search_wolframmodel.wls; its core is reproduced here. *)
wm = ResourceFunction["WolframModel"];
cand = {                                            (* {rule, init} candidates *)
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
results = Table[
  Module[{rule = cand[[i, 1]], init = cand[[i, 2]], vs, ratios, dGrowth, regime},
   vs = Table[TimeConstrained[
       Length[Union[Flatten[wm[rule, init, g]["FinalState"]]]], 8, Missing[]], {g, gens}];
   If[MemberQ[vs, Missing[] | _?(# > 60000 &)], regime = "explodes"; dGrowth = Infinity,
    ratios = Differences[Log[N@vs]]/Differences[N@gens];   (* d log V / d gen *)
    dGrowth = lsSlope[Log[N@gens], Log[N@vs]];
    regime = Which[Max[vs] < 12, "trivial", Mean[ratios] > 0.35, "exponential",
      dGrowth < 1.4, "~1D (linear)", True, "candidate"]];
   {i, regime, dGrowth, Last[vs], vs}], {i, Length[cand]}];

(* the "missing middle": counts by regime, and the bracketing growth curves *)
regimes = {"trivial", "~1D (linear)", "candidate", "exponential"};
counts = (Count[results[[All, 2]], #] &) /@ regimes;
barP = BarChart[counts, ChartLabels -> {"trivial", "~1D", "intermediate", "exponential\n(hyperbolic)"},
   ChartStyle -> {Gray, RGBColor[0.15, 0.35, 0.8], RGBColor[0.2, 0.6, 0.2], RGBColor[0.85, 0.3, 0.1]},
   PlotLabel -> Style["Emergent geometry of 20 rules: the missing middle", 13],
   AxesLabel -> {None, "# rules"}, ImageSize -> 430];
rep1D = SelectFirst[results, #[[2]] == "~1D (linear)" &];
repExp = SelectFirst[results, #[[2]] == "exponential" &];
gg = Range[3, 11];
curveP = ListLogPlot[{Transpose[{gens, rep1D[[5]]}], Transpose[{gens, repExp[[5]]}],
    Transpose[{gg, gg^2}], Transpose[{gg, gg^3}]},
   Joined -> True, PlotMarkers -> {Automatic, Automatic, None, None},
   PlotStyle -> {RGBColor[0.15, 0.35, 0.8], RGBColor[0.85, 0.3, 0.1],
      Directive[Gray, Dashed], Directive[Gray, Dotted]},
   PlotLegends -> Placed[{"a 1D rule", "an exponential rule", "g^2 (2D)", "g^3 (3D)"}, Below],
   Frame -> True, FrameLabel -> {"generation g", "vertices V(g)"}, ImageSize -> 430,
   GridLines -> Automatic];
GraphicsRow[{barP, curveP}, Spacings -> 30]
