#!/usr/bin/env wolframscript
(* meancurvature.wls — cotangent Laplace-Beltrami operator + mean curvature.

   1. Build the cotangent Laplacian L and sanity-check it: symmetric, and
      constants lie in its kernel (L.1 = 0).
   2. Mean-curvature normal  -(L.x)/(2A) = kappaH * n  (Meyer et al. 2003).
      Validate |kappaH| against the smooth value on a unit sphere (H = 1) and
      on a torus (H(u) = (R + 2 r cos u)/(2 r (R + r cos u))).
   3. Render the torus coloured by signed mean curvature -> docs/images/.    *)

Needs["DDG`", FileNameJoin[{DirectoryName[$InputFileName], "ddg.wl"}]];

sci[x_] := If[x == 0., "0",
  With[{e = Floor[Log10[Abs[x]]]},
    ToString[NumberForm[N[x]/10.^e, {4, 3}]] <> "e" <> ToString[e]]];

(* ---- operator sanity checks on a regular geodesic icosphere ------------ *)
sphere = DDG`icosphereMesh[5];     (* icosahedron subdivided 5x -> unit sphere *)
Lsph = DDG`cotanLaplacian[sphere];
nV = Length[DDG`meshCoords[sphere]];
symErr = Max[Abs[Lsph - Transpose[Lsph]]];
kernelErr = Max[Abs[Lsph . ConstantArray[1., nV]]];
Print["cotan Laplacian on icosphere(5):  ", nV, " vertices"];
Print["  symmetry  max|L - L^T|        = ", sci[symErr]];
Print["  constants in kernel max|L.1|  = ", sci[kernelErr]];

(* ---- mean curvature on the unit sphere (H = 1) ------------------------- *)
Hsph = DDG`meanCurvature[sphere];                  (* signed *)
sgn = Sign[Mean[Hsph]];                            (* fix winding/normal sign *)
HsphMag = sgn Hsph;
Print["sphere mean curvature (exact H = 1):"];
Print["  mean = ", sci[Mean[HsphMag]],
  "   median = ", sci[Median[HsphMag]],
  "   rel L2 err vs 1 = ", sci[Norm[HsphMag - 1.]/Sqrt[nV]]];

(* ---- mean curvature on the torus --------------------------------------- *)
R = 2.; r = 0.8;
torus = DDG`torusMesh[R, r, 80, 160];
uvals = DDG`torusAngleU[DDG`meshCoords[torus], R, r];
Hexact = (R + 2 r Cos[uvals])/(2 r (R + r Cos[uvals]));
Hdisc0 = DDG`meanCurvature[torus];
tsgn = Sign[Mean[Hdisc0 Hexact]];                  (* align global sign *)
Hdisc = tsgn Hdisc0;
relErr = Norm[Hdisc - Hexact]/Norm[Hexact];
Print["torus mean curvature  discrete vs smooth:"];
Print["  rel L2 error = ", sci[relErr]];
Print["  H range  discrete = ", MinMax[Hdisc], "   smooth = ", MinMax[Hexact]];

(* ---- render torus coloured by signed mean curvature -------------------- *)
hmax = Max[Abs[Hdisc]];
colors = Map[ColorData["TemperatureMap"], Rescale[Hdisc, {-hmax, hmax}]];
fig = Graphics3D[
   {EdgeForm[], GraphicsComplex[DDG`meshCoords[torus],
      Polygon[DDG`meshTriangles[torus]], VertexColors -> colors]},
   Boxed -> False, ViewPoint -> {2.2, -2.4, 1.8}, ImageSize -> 900,
   Lighting -> "Neutral",
   PlotLabel -> Style["Discrete mean curvature on a torus (cotan Laplacian)", 14]];
out = FileNameJoin[{ParentDirectory[DirectoryName[$InputFileName]],
    "docs", "images", "torus_mean_curvature.png"}];
Export[out, fig, ImageResolution -> 120];
Print["wrote ", out];
