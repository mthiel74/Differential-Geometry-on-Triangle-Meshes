#!/usr/bin/env wolframscript
(* curvature.wls — first DDG exploration.

   1. Build a torus triangle mesh (closed, genus 1, chi = 0) from a periodic
      parametric grid.
   2. Compute per-vertex angle-defect Gaussian curvature and check it against
      the smooth analytic torus curvature.
   3. Verify the discrete Gauss-Bonnet theorem  sum(angle defect) = 2*Pi*chi
      on both the torus (chi = 0) and a discretized sphere (chi = 2 -> 4*Pi).
   4. Render the torus coloured by Gaussian curvature -> docs/images/.        *)

Needs["DDG`", FileNameJoin[{DirectoryName[$InputFileName], "ddg.wl"}]];

(* single-line scientific string (ScientificForm typesets across two lines
   under wolframscript) *)
sci[x_] := If[x == 0., "0",
  With[{e = Floor[Log10[Abs[x]]]},
    ToString[NumberForm[N[x]/10.^e, {4, 3}]] <> "e" <> ToString[e]]];

(* ---- build a torus mesh from a periodic (u,v) grid --------------------- *)
(* R = tube-centre radius, r = tube radius. Smooth Gaussian curvature is
   K(u,v) = cos(u) / (r (R + r cos(u))), u = angle around the tube.          *)
R = 2.; r = 0.8;
torus = DDG`torusMesh[R, r, 80, 160];     (* nu x nv grid *)
coords = DDG`meshCoords[torus];
tris = DDG`meshTriangles[torus];
Print["torus: ", Length[coords], " vertices, ", Length[tris], " triangles"];

(* ---- discrete vs smooth Gaussian curvature on the torus ---------------- *)
uvals = DDG`torusAngleU[coords, R, r];
Kdiscrete = DDG`gaussianCurvature[torus];
Ksmooth = Cos[uvals]/(r (R + r Cos[uvals]));

relErr = Norm[Kdiscrete - Ksmooth]/Norm[Ksmooth];
Print["Gaussian curvature  discrete vs smooth  relative L2 error = ", sci[relErr]];
Print["  K range  discrete = ", MinMax[Kdiscrete],
  "   smooth = ", MinMax[Ksmooth]];

(* ---- Gauss-Bonnet on torus (chi = 0) ----------------------------------- *)
gbTorus = DDG`gaussBonnetCheck[torus];
Print["Gauss-Bonnet (torus):  total defect = ", sci[gbTorus["totalDefect"]],
  "   2*Pi*chi = ", gbTorus["twoPiChi"], "   chi = ", gbTorus["chi"]];

(* ---- Gauss-Bonnet on a discretized sphere (chi = 2 -> 4 Pi) ------------ *)
sphere = DiscretizeRegion[Sphere[{0, 0, 0}, 1], MaxCellMeasure -> {1 -> 0.05}];
gbSphere = DDG`gaussBonnetCheck[sphere];
Print["Gauss-Bonnet (sphere): total defect = ", gbSphere["totalDefect"],
  "   4*Pi = ", N[4 Pi], "   chi = ", gbSphere["chi"],
  "   residual = ", sci[gbSphere["residual"]]];

(* ---- render torus coloured by Gaussian curvature ----------------------- *)
(* diverging map: blue = negative (inner rim), red = positive (outer rim) *)
cmax = Max[Abs[Kdiscrete]];
colors = Map[ColorData["TemperatureMap"], Rescale[Kdiscrete, {-cmax, cmax}]];

fig = Graphics3D[
   {EdgeForm[], GraphicsComplex[coords, Polygon[tris], VertexColors -> colors]},
   Boxed -> False, ViewPoint -> {2.2, -2.4, 1.8}, ImageSize -> 900,
   Lighting -> "Neutral",
   PlotLabel -> Style["Discrete Gaussian curvature on a torus (angle defect)", 14]];

outdir = FileNameJoin[{ParentDirectory[DirectoryName[$InputFileName]], "docs", "images"}];
out = FileNameJoin[{outdir, "torus_gaussian_curvature.png"}];
Export[out, fig, ImageResolution -> 120];
Print["wrote ", out];
