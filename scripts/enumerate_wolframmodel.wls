#!/usr/bin/env wolframscript
(* enumerate_wolframmodel.wls — map the distribution of emergent dimension
   across many Wolfram-model rules.

   We generate a reproducible sample of rules (several LHS templates x random
   RHS rewrites), evolve each a few generations, and read the emergent
   "growth dimension" from how the vertex count scales:
       V(g) ~ g^d   (d-dimensional space)   vs   V(g) ~ c^g   (exponential).
   We sample V at g = 3,5,7,9 and DISCRIMINATE the two models by fit quality:
   log V linear in g  => exponential;  log V linear in log g  => polynomial
   (dimension d = slope). A 2-point growth exponent cannot tell an
   exponential with a small base from a power law over a short window, so the
   fit comparison is essential. The histogram shows how rare flat finite d>1 is.

   One-off, needs internet (ResourceFunction). Writes a figure + a CSV.        *)

repo = ParentDirectory[DirectoryName[$InputFileName]];
imgDir = FileNameJoin[{repo, "docs", "images"}];
wm = ResourceFunction["WolframModel"];

SeedRandom[42];
templates = {{{1, 2}}, {{1, 2}, {2, 3}}, {{1, 2, 3}}, {{1, 2, 3}, {3, 4, 5}}};
genRHS[lhs_] := Module[{a = Length[lhs[[1]]], atoms, pool, nr},
  atoms = Union[Flatten[lhs]];
  pool = Join[atoms, Max[atoms] + Range[3]];     (* allow up to 3 new atoms *)
  nr = RandomInteger[{2, 4}];
  Table[RandomChoice[pool, a], {nr}]];
rules = DeleteDuplicates@Flatten[
   Table[With[{lhs = t}, Table[lhs -> genRHS[lhs], {40}]], {t, templates}], 1];
Print["generated ", Length[rules], " rules"];

initFor[lhs_] := lhs /. n_Integer :> 1;          (* collapse to a seed state *)

vAt[rule_, g_] := TimeConstrained[
   Length[Union[Flatten[wm[rule, initFor[rule[[1]]], g]["FinalState"]]]], 5, Missing[]];

(* R^2 of a 1-variable linear fit y ~ a + b x *)
r2[xs_, ys_] := Module[{m = Length[xs], b, a, pred},
  b = (m Total[xs ys] - Total[xs] Total[ys])/(m Total[xs^2] - Total[xs]^2);
  a = Mean[ys] - b Mean[xs]; pred = a + b xs;
  1 - Total[(ys - pred)^2]/Total[(ys - Mean[ys])^2]];
slope[xs_, ys_] := With[{m = Length[xs]},
  (m Total[xs ys] - Total[xs] Total[ys])/(m Total[xs^2] - Total[xs]^2)];

gsamp = {3, 5, 7, 9};
Print["screening (V at g = 3,5,7,9; exponential vs power-law fit) ..."];
data = Table[
   Module[{r = rules[[i]], vs, lg, lv, rExp, rPoly, d, regime},
    vs = Table[If[# === Missing[] || # > 30000, Missing[], #] &@vAt[r, g], {g, gsamp}];
    Which[
     MemberQ[vs, Missing[]], regime = "exponential"; d = Missing[],
     Max[vs] < 8, regime = "trivial"; d = 0.,
     True,
      lg = Log[N@gsamp]; lv = Log[N@vs];
      rExp = r2[N@gsamp, lv];          (* log V ~ g  => exponential *)
      rPoly = r2[lg, lv];              (* log V ~ log g => power law *)
      d = slope[lg, lv];
      regime = Which[
        rExp >= rPoly || d > 3.8, "exponential",
        d < 1.35, "~1D", d < 2.5, "~2D", True, "~3D"]];
    {i, regime, d, Last[vs]}],
   {i, Length[rules]}];

regimes = {"trivial", "~1D", "~2D", "~3D", "exponential"};
counts = (Count[data[[All, 2]], #] &) /@ regimes;
Print["\nregime counts:"];
MapThread[Print["  ", StringPadRight[#1, 12], #2] &, {regimes, counts}];
finiteD = Cases[data, {_, r_ /; MemberQ[{"~1D", "~2D", "~3D"}, r], d_, _} :> d];
Print["finite-dimension rules: ", Length[finiteD],
  " of ", Length[rules], "  (median d = ",
  If[finiteD =!= {}, NumberForm[Median[finiteD], {3, 2}], "-"], ")"];
Print["flat 2D/3D (d in [1.7,3.5]): ", Count[finiteD, d_ /; 1.7 <= d <= 3.5]];

(* CSV record *)
Export[FileNameJoin[{repo, "data", "wm_rule_distribution.csv"}],
   Prepend[data /. Missing[] -> "", {"rule", "regime", "d_growth", "V7"}]];

(* figure: histogram of the FINITE-dimensional rules only (exponential rules
   are counted in the bar chart, not binned by their spurious slope) *)
hist = Histogram[finiteD, {0, 3.5, 0.25}, ChartStyle -> RGBColor[0.2, 0.45, 0.75],
   Frame -> True, FrameLabel -> {"growth dimension d (finite-d rules only)", "# rules"},
   PlotLabel -> Style["Where the finite-dimensional rules land", 13],
   Epilog -> {RGBColor[0.1, 0.5, 0.2], Dashed,
     Line[{{1.7, 0}, {1.7, 14}}], Line[{{3.5, 0}, {3.5, 14}}],
     Text[Style["flat 2D/3D band", Italic, 10, Darker@Green], {2.6, 8}]},
   PlotRange -> {{0, 3.6}, All}, ImageSize -> 460];
bar = BarChart[counts, ChartLabels -> regimes,
   ChartStyle -> {Gray, RGBColor[0.15, 0.35, 0.8], RGBColor[0.2, 0.6, 0.2],
      RGBColor[0.6, 0.4, 0.1], RGBColor[0.85, 0.3, 0.1]},
   Frame -> True, FrameLabel -> {None, "# rules"},
   PlotLabel -> Style["Regime census", 13], ImageSize -> 460];
Export[FileNameJoin[{imgDir, "wm_rule_distribution.png"}],
   GraphicsRow[{hist, bar}, ImageSize -> 940, Spacings -> 30], ImageResolution -> 120];
Print["wrote wm_rule_distribution.png + data/wm_rule_distribution.csv"];
