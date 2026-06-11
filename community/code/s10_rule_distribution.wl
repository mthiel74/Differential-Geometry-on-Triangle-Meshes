(* Scaling the screen to 160 rules, discriminating exponential from power-law
   growth by fit quality (a 2-point exponent cannot). One-off, needs internet.
   Full driver: scripts/enumerate_wolframmodel.wls; its core is reproduced here. *)
wm = ResourceFunction["WolframModel"];
SeedRandom[42];
templates = {{{1, 2}}, {{1, 2}, {2, 3}}, {{1, 2, 3}}, {{1, 2, 3}, {3, 4, 5}}};
genRHS[lhs_] := Module[{a = Length[lhs[[1]]], atoms, pool, nr},
  atoms = Union[Flatten[lhs]];
  pool = Join[atoms, Max[atoms] + Range[3]];        (* up to 3 new atoms *)
  nr = RandomInteger[{2, 4}];
  Table[RandomChoice[pool, a], {nr}]];
rules = DeleteDuplicates@Flatten[
   Table[With[{lhs = t}, Table[lhs -> genRHS[lhs], {40}]], {t, templates}], 1];
initFor[lhs_] := lhs /. n_Integer :> 1;

vAt[rule_, g_] := TimeConstrained[
   Length[Union[Flatten[wm[rule, initFor[rule[[1]]], g]["FinalState"]]]], 5, Missing[]];
r2[xs_, ys_] := Module[{m = Length[xs], b, a, pred},
  b = (m Total[xs ys] - Total[xs] Total[ys])/(m Total[xs^2] - Total[xs]^2);
  a = Mean[ys] - b Mean[xs]; pred = a + b xs;
  1 - Total[(ys - pred)^2]/Total[(ys - Mean[ys])^2]];
slope[xs_, ys_] := With[{m = Length[xs]},
  (m Total[xs ys] - Total[xs] Total[ys])/(m Total[xs^2] - Total[xs]^2)];

gsamp = {3, 5, 7, 9};
data = Table[
   Module[{r = rules[[i]], vs, lg, lv, rExp, rPoly, d, regime},
    vs = Table[If[# === Missing[] || # > 30000, Missing[], #] &@vAt[r, g], {g, gsamp}];
    Which[
     MemberQ[vs, Missing[]], regime = "exponential"; d = Missing[],
     Max[vs] < 8, regime = "trivial"; d = 0.,
     True,
      lg = Log[N@gsamp]; lv = Log[N@vs];
      rExp = r2[N@gsamp, lv];          (* log V ~ g     => exponential *)
      rPoly = r2[lg, lv];              (* log V ~ log g  => power law *)
      d = slope[lg, lv];
      regime = Which[rExp >= rPoly || d > 3.8, "exponential",
        d < 1.35, "~1D", d < 2.5, "~2D", True, "~3D"]];
    {i, regime, d, Last[vs]}], {i, Length[rules]}];

regimes = {"trivial", "~1D", "~2D", "~3D", "exponential"};
counts = (Count[data[[All, 2]], #] &) /@ regimes;
finiteD = Cases[data, {_, r_ /; MemberQ[{"~1D", "~2D", "~3D"}, r], d_, _} :> d];
hist = Histogram[finiteD, {0, 3.5, 0.25}, ChartStyle -> RGBColor[0.2, 0.45, 0.75],
   Frame -> True, FrameLabel -> {"growth dimension d (finite-d rules only)", "# rules"},
   PlotLabel -> Style["Where the finite-dimensional rules land", 13],
   Epilog -> {RGBColor[0.1, 0.5, 0.2], Dashed, Line[{{1.7, 0}, {1.7, 14}}], Line[{{3.5, 0}, {3.5, 14}}],
     Text[Style["flat 2D/3D band", Italic, 10, Darker@Green], {2.6, 8}]},
   PlotRange -> {{0, 3.6}, All}, ImageSize -> 460];
bar = BarChart[counts, ChartLabels -> regimes,
   ChartStyle -> {Gray, RGBColor[0.15, 0.35, 0.8], RGBColor[0.2, 0.6, 0.2],
      RGBColor[0.6, 0.4, 0.1], RGBColor[0.85, 0.3, 0.1]},
   Frame -> True, FrameLabel -> {None, "# rules"}, PlotLabel -> Style["Regime census", 13],
   ImageSize -> 460];
GraphicsRow[{hist, bar}, Spacings -> 30]
