#!/usr/bin/env wolframscript
(* heat.wls — geodesic distance by the heat method (Crane, Weischedel &
   Wardetzky 2013), built on the same cotangent Laplacian.

   Algorithm: (1) diffuse heat from the source for a short time t~h^2 by one
   backward-Euler step (M - t L) u = delta; (2) the normalised gradient
   X = -grad u/|grad u| is a unit field pointing away from the source;
   (3) the geodesic distance is the scalar whose gradient best matches X,
   recovered from the Poisson problem (-L) phi = div X.

   1. Validation + convergence: on the unit sphere the distance from the
      north pole equals the colatitude arccos(z); the error shrinks under
      refinement.
   2. Figures: banded geodesic-distance fields on a sphere and a torus.    *)

Needs["DDG`", FileNameJoin[{DirectoryName[$InputFileName], "ddg.wl"}]];

imgDir = FileNameJoin[{ParentDirectory[DirectoryName[$InputFileName]], "docs", "images"}];
sci[x_] := If[x == 0., "0",
  With[{e = Floor[Log10[Abs[x]]]},
    ToString[NumberForm[N[x]/10.^e, {4, 3}]] <> "e" <> ToString[e]]];

(* ---- 1. validation + convergence on the sphere ------------------------- *)
Print["heat-method geodesics: distance from the north pole vs arccos(z)"];
Do[
  Module[{sph = DDG`icosphereMesh[lev], pts, src, phi, exact},
   pts = DDG`meshCoords[sph];
   src = First@Ordering[pts[[All, 3]], -1];
   phi = DDG`heatGeodesics[sph, src];
   exact = ArcCos[Clip[pts[[All, 3]], {-1., 1.}]];
   Print["  icosphere(", lev, ")  n=", Length[pts],
     "   rel L2 error = ", sci[Norm[phi - exact]/Norm[exact]]]],
  {lev, {3, 4, 5}}];

(* ---- banded geodesic-distance renderer --------------------------------- *)
renderGeo[mr_, phi_, nb_, view_, lbl_] := Module[
   {pts = DDG`meshCoords[mr], tris = DDG`meshTriangles[mr], phase, cols},
   phase = phi/Max[phi];
   cols = (ColorData["Rainbow"][Round[# nb]/nb] &) /@ phase;  (* contour bands *)
   Graphics3D[{EdgeForm[],
      GraphicsComplex[pts, Polygon[tris], VertexColors -> cols]},
     Boxed -> False, ViewPoint -> view, ImageSize -> 480, Lighting -> "Neutral",
     PlotLabel -> Style[lbl, 12]]];

(* sphere from the north pole: bands are circles of latitude *)
sph = DDG`icosphereMesh[5]; spts = DDG`meshCoords[sph];
ssrc = First@Ordering[spts[[All, 3]], -1];
sphi = DDG`heatGeodesics[sph, ssrc];

(* torus from a point on the outer equator: bands wrap the tube and the hole *)
R = 2.; r = 0.8;
torus = DDG`torusMesh[R, r, 80, 160]; tpts = DDG`meshCoords[torus];
tsrc = First@Ordering[tpts[[All, 1]], -1];     (* extreme +x: outer equator *)
tphi = DDG`heatGeodesics[torus, tsrc];

out = FileNameJoin[{imgDir, "geodesics.png"}];
Export[out,
  GraphicsRow[{
     renderGeo[sph, sphi, 16, {0, -2.5, 1.3},
       "geodesic distance on a sphere (from the pole)"],
     renderGeo[torus, tphi, 18, {2.2, -2.4, 1.8},
       "geodesic distance on a torus (from one point)"]},
    Spacings -> 0, ImageSize -> 1040], ImageResolution -> 120];
Print["wrote ", out];
