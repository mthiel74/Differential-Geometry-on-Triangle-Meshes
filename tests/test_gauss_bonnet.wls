#!/usr/bin/env wolframscript
(* test_gauss_bonnet.wls — sanity check: discrete Gauss-Bonnet holds on a
   closed discretized sphere, total angle defect = 2*Pi*chi = 4*Pi.        *)

Needs["DDG`", FileNameJoin[{ParentDirectory[DirectoryName[$InputFileName]], "wolfram", "ddg.wl"}]];

sphere = DiscretizeRegion[Sphere[{0, 0, 0}, 1], MaxCellMeasure -> {1 -> 0.05}];
gb = DDG`gaussBonnetCheck[sphere];

tol = 1.*^-8;
pass = Abs[gb["residual"]] < tol && gb["chi"] == 2;

Print["chi = ", gb["chi"], "   total defect = ", gb["totalDefect"],
  "   residual = ", gb["residual"]];
Print[If[pass, "PASS", "FAIL"], "  (tol = ", tol, ")"];

Exit[If[pass, 0, 1]];
