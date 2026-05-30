#!/usr/bin/env wolframscript
(* test_convergence.wls — the discrete operators converge at the expected
   order: curvature ~ O(h^2), the heat method sub-linear but positive, and the
   Gauss-Bonnet residual at machine epsilon regardless of resolution. *)

Needs["DDG`", FileNameJoin[{ParentDirectory[DirectoryName[$InputFileName]], "wolfram", "ddg.wl"}]];

lsSlope[xs_, ys_] := With[{n = Length[xs]},
  (n Total[xs ys] - Total[xs] Total[ys])/(n Total[xs^2] - Total[xs]^2)];

R = 2.; r = 0.8;
cur = Table[Module[{m = DDG`torusMesh[R, r, res[[1]], res[[2]]], h, u, Kd, Ks, gb},
    h = DDG`meanEdgeLength[m];
    u = DDG`torusAngleU[DDG`meshCoords[m], R, r];
    Kd = DDG`gaussianCurvature[m]; Ks = Cos[u]/(r (R + r Cos[u]));
    gb = DDG`gaussBonnetCheck[m];
    {h, Norm[Kd - Ks]/Norm[Ks], Abs[gb["residual"]]}],
   {res, {{20, 40}, {40, 80}, {80, 160}}}];
pK = lsSlope[Log[cur[[All, 1]]], Log[cur[[All, 2]]]];
gbMax = Max[cur[[All, 3]]];

geo = Table[Module[{m = DDG`icosphereMesh[lev], pts, src, phi, exact, h},
    pts = DDG`meshCoords[m]; h = DDG`meanEdgeLength[m];
    src = First@Ordering[pts[[All, 3]], -1];
    phi = DDG`heatGeodesics[m, src];
    exact = ArcCos[Clip[pts[[All, 3]], {-1., 1.}]];
    {h, Norm[phi - exact]/Norm[exact]}], {lev, 2, 4}];
pGeo = lsSlope[Log[geo[[All, 1]]], Log[geo[[All, 2]]]];

pass = 1.7 < pK < 2.3 && 0.3 < pGeo < 1.3 && gbMax < 1.*^-9;
Print["curvature order p = ", pK, "   geodesic order p = ", pGeo,
  "   max Gauss-Bonnet residual = ", gbMax];
Print[If[pass, "PASS", "FAIL"]];
Exit[If[pass, 0, 1]];
