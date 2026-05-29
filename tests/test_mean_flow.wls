#!/usr/bin/env wolframscript
(* test_mean_flow.wls — under mean-curvature flow a round sphere obeys
   R(t)^2 = R0^2 - 4 t. The semi-implicit DDG`implicitFairing must recover
   the slope d(R^2)/dt = -4. *)

Needs["DDG`", FileNameJoin[{ParentDirectory[DirectoryName[$InputFileName]], "wolfram", "ddg.wl"}]];

base = DDG`icosphereMesh[4];
tris = DDG`meshTriangles[base];
tau = 2.*^-3; n = 12;
frames = DDG`implicitFairing[DDG`meshCoords[base], tris, tau, n];
radii = (Mean[Norm /@ #] &) /@ frames;
ts = tau Range[0, n];
lsSlope[xs_, ys_] := With[{m = Length[xs]},
  (m Total[xs ys] - Total[xs] Total[ys])/(m Total[xs^2] - Total[xs]^2)];
slope = lsSlope[ts, radii^2];

pass = Abs[(slope + 4)/4] < 0.02;
Print["fitted d(R^2)/dt = ", slope, "   (theory -4)"];
Print[If[pass, "PASS", "FAIL"]];
Exit[If[pass, 0, 1]];
