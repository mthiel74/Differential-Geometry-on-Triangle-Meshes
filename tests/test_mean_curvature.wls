#!/usr/bin/env wolframscript
(* test_mean_curvature.wls — the cotangent mean-curvature operator recovers
   the smooth mean curvature: H = 1 on a unit icosphere, and H(u) on a torus. *)

Needs["DDG`", FileNameJoin[{ParentDirectory[DirectoryName[$InputFileName]], "wolfram", "ddg.wl"}]];

(* unit sphere: exact mean curvature H = 1 *)
sphere = DDG`icosphereMesh[4];
Hs = DDG`meanCurvature[sphere];
Hs = Sign[Mean[Hs]] Hs;                            (* align global sign *)
sphereErr = Norm[Hs - 1.]/Sqrt[Length[Hs]];

(* torus: exact H(u) = (R + 2 r cos u)/(2 r (R + r cos u)) *)
R = 2.; r = 0.8;
torus = DDG`torusMesh[R, r, 60, 120];
uvals = DDG`torusAngleU[DDG`meshCoords[torus], R, r];
Hexact = (R + 2 r Cos[uvals])/(2 r (R + r Cos[uvals]));
Hd = DDG`meanCurvature[torus];
Hd = Sign[Mean[Hd Hexact]] Hd;
torusErr = Norm[Hd - Hexact]/Norm[Hexact];

pass = sphereErr < 0.02 && torusErr < 0.01;
Print["sphere rel L2 err vs H=1 = ", sphereErr];
Print["torus  rel L2 err vs H(u) = ", torusErr];
Print[If[pass, "PASS", "FAIL"]];
Exit[If[pass, 0, 1]];
