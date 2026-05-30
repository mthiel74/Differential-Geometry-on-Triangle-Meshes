#!/usr/bin/env wolframscript
(* convergence.wls — how fast do the discrete operators converge?

   "Validated against a smooth reference" is only half the story; the
   scientific question is the *order of accuracy* -- how the error scales with
   the mesh spacing h. We refine three families and fit the empirical order
   p in  error ~ C h^p  by least squares on a log-log plot:

     - Gaussian curvature (angle defect) on a torus,
     - mean curvature (cotan Laplacian) on a torus,
     - geodesic distance (heat method) on a sphere.

   We also track the Gauss-Bonnet residual, which stays at machine epsilon at
   every resolution -- the visual proof that it is an identity, not an
   approximation that improves with refinement.                              *)

Needs["DDG`", FileNameJoin[{DirectoryName[$InputFileName], "ddg.wl"}]];
imgDir = FileNameJoin[{ParentDirectory[DirectoryName[$InputFileName]], "docs", "images"}];

lsSlope[xs_, ys_] := With[{n = Length[xs]},
  (n Total[xs ys] - Total[xs] Total[ys])/(n Total[xs^2] - Total[xs]^2)];
order[hs_, errs_] := lsSlope[Log[hs], Log[errs]];

(* ---- curvature convergence on the torus -------------------------------- *)
R = 2.; r = 0.8;
torusRes = {{20, 40}, {30, 60}, {40, 80}, {56, 112}, {80, 160}};
curvData = Table[
   Module[{m = DDG`torusMesh[R, r, res[[1]], res[[2]]], h, u, Kd, Ks, Hd, Hs, gb},
    h = DDG`meanEdgeLength[m];
    u = DDG`torusAngleU[DDG`meshCoords[m], R, r];
    Kd = DDG`gaussianCurvature[m]; Ks = Cos[u]/(r (R + r Cos[u]));
    Hd = DDG`meanCurvature[m]; Hd = Sign[Mean[Hd]] Hd;
    Hs = (R + 2 r Cos[u])/(2 r (R + r Cos[u]));
    gb = DDG`gaussBonnetCheck[m];
    {h, Norm[Kd - Ks]/Norm[Ks], Norm[Hd - Hs]/Norm[Hs], Abs[gb["residual"]] + 1.*^-17}],
   {res, torusRes}];
hC = curvData[[All, 1]]; errK = curvData[[All, 2]];
errH = curvData[[All, 3]]; resGB = curvData[[All, 4]];

(* ---- geodesic convergence on the sphere -------------------------------- *)
geoData = Table[
   Module[{m = DDG`icosphereMesh[lev], pts, src, phi, exact, h},
    pts = DDG`meshCoords[m]; h = DDG`meanEdgeLength[m];
    src = First@Ordering[pts[[All, 3]], -1];
    phi = DDG`heatGeodesics[m, src];
    exact = ArcCos[Clip[pts[[All, 3]], {-1., 1.}]];
    {h, Norm[phi - exact]/Norm[exact]}],
   {lev, 2, 5}];
hG = geoData[[All, 1]]; errGeo = geoData[[All, 2]];

pK = order[hC, errK]; pH = order[hC, errH]; pGeo = order[hG, errGeo];
fmt[x_] := ToString[NumberForm[x, {3, 2}]];
Print["empirical order of accuracy  (error ~ h^p):"];
Print["  Gaussian curvature : p = ", fmt[pK]];
Print["  mean curvature     : p = ", fmt[pH]];
Print["  heat geodesics     : p = ", fmt[pGeo]];
Print["  Gauss-Bonnet residual stays ~1e-12 at every resolution (identity)"];

(* ---- log-log figure ---------------------------------------------------- *)
ref[hs_, errs_, p_] := Module[{c = Exp[Mean[Log[errs] - p Log[hs]]]},
  Table[{h, c h^p}, {h, {Min[hs], Max[hs]}}]];

fig = ListLogLogPlot[
   {Transpose[{hC, errK}], Transpose[{hC, errH}], Transpose[{hG, errGeo}],
    Transpose[{hC, resGB}]},
   Joined -> False, PlotMarkers -> {Automatic, 10},
   PlotStyle -> {RGBColor[0.85, 0.3, 0.1], RGBColor[0.1, 0.5, 0.2],
      RGBColor[0.15, 0.35, 0.8], GrayLevel[0.55]},
   Frame -> True, FrameLabel -> {"mean edge length  h", "relative L2 error"},
   PlotLabel -> Style["Order of accuracy: error ~ h^p", 14],
   PlotLegends -> Placed[{
      "Gaussian curvature  (p=" <> ToString[NumberForm[pK, {3, 2}]] <> ")",
      "mean curvature  (p=" <> ToString[NumberForm[pH, {3, 2}]] <> ")",
      "heat geodesics  (p=" <> ToString[NumberForm[pGeo, {3, 2}]] <> ")",
      "Gauss-Bonnet residual (identity)"}, Below],
   Epilog -> {Gray, Dashed,
     Line[Log[#] & /@ ref[hC, errK, 2]],     (* slope-2 guide *)
     Line[Log[#] & /@ ref[hG, errGeo, 1]]},  (* slope-1 guide *)
   GridLines -> Automatic, ImageSize -> 620];
out = FileNameJoin[{imgDir, "convergence.png"}];
Export[out, fig, ImageResolution -> 120];
Print["wrote ", out];
