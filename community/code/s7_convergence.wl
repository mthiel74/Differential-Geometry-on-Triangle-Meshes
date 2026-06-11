(* Order of accuracy: refine the test surfaces and fit error ~ C h^p by
   log-log least squares. Curvature on the torus, geodesics on the sphere,
   and the Gauss-Bonnet residual (which stays at machine epsilon: an identity). *)
lsSlope[xs_, ys_] := With[{n = Length[xs]},
  (n Total[xs ys] - Total[xs] Total[ys])/(n Total[xs^2] - Total[xs]^2)];
order[hs_, errs_] := lsSlope[Log[hs], Log[errs]];

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
geoData = Table[
   Module[{m = DDG`icosphereMesh[lev], p, src, phi, exact},
    p = DDG`meshCoords[m]; src = First@Ordering[p[[All, 3]], -1];
    phi = DDG`heatGeodesics[m, src]; exact = ArcCos[Clip[p[[All, 3]], {-1., 1.}]];
    {DDG`meanEdgeLength[m], Norm[phi - exact]/Norm[exact]}], {lev, 2, 5}];
{hC, errK, errH, resGB} = Transpose[curvData];
{hG, errGeo} = Transpose[geoData];

ListLogLogPlot[{Transpose[{hC, errK}], Transpose[{hC, errH}],
    Transpose[{hG, errGeo}], Transpose[{hC, resGB}]},
   Joined -> False, PlotMarkers -> {Automatic, 10}, Frame -> True,
   FrameLabel -> {"mean edge length  h", "relative L2 error"},
   PlotLegends -> {
     "Gaussian curvature  (p=" <> ToString[NumberForm[order[hC, errK], {3, 2}]] <> ")",
     "mean curvature  (p=" <> ToString[NumberForm[order[hC, errH], {3, 2}]] <> ")",
     "heat geodesics  (p=" <> ToString[NumberForm[order[hG, errGeo], {3, 2}]] <> ")",
     "Gauss-Bonnet residual (identity)"},
   ImageSize -> 620, GridLines -> Automatic]
