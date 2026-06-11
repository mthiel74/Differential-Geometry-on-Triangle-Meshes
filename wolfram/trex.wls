#!/usr/bin/env wolframscript
(* trex.wls — the DDG operators on a real 3D scan.

   A 12k-triangle decimation of a photogrammetric museum-cast scan of a
   Tyrannosaurus rex skull (CC0; Emily Hauf / Digital Atlas of Ancient Life;
   see data/CREDITS.md and scripts/prepare_trex.py) stands in for the textbook
   sphere/torus, showing that the same operators run on messy real geometry:

   1. discrete mean curvature  -> docs/images/trex_mean_curvature.png
   2. heat-method geodesic distance from the tip of the snout
                                 -> docs/images/trex_geodesics.png
   3. Laplace-Beltrami eigenfunctions (manifold harmonics) on the coarse mesh
                                 -> docs/images/trex_eigenfunctions.png        *)

Needs["DDG`", FileNameJoin[{DirectoryName[$InputFileName], "ddg.wl"}]];

dataDir = FileNameJoin[{ParentDirectory[DirectoryName[$InputFileName]], "data"}];
imgDir  = FileNameJoin[{ParentDirectory[DirectoryName[$InputFileName]], "docs", "images"}];

(* lateral "museum" profile, a touch of 3/4 for depth; long axis is z,
   dorsal is +x, so we look along -y with +x up *)
vp = {0.55, -2.25, 0.05}; vv = {1, 0, 0};

trex = Import[FileNameJoin[{dataDir, "trex.obj"}], "MeshRegion"];
pts = DDG`meshCoords[trex]; tris = DDG`meshTriangles[trex];
Print["trex.obj: ", Length[pts], " vertices, ", Length[tris], " triangles"];

(* snout = the narrower end of the long (z) axis; source there so distance
   creeps back over the cranium, as an ant on the surface would travel *)
zc = pts[[All, 3]]; {zlo, zhi} = MinMax[zc]; zlen = zhi - zlo;
spread[end_] := Module[{xy = Select[pts, Abs[#[[3]] - end] < 0.12 zlen &][[All, {1, 2}]]},
   RootMeanSquare[Flatten[(# - Mean[xy]) & /@ xy]]];
snoutZ = If[spread[zlo] < spread[zhi], zlo, zhi];
src = First@Ordering[Abs[zc - snoutZ], 1];
Print["snout at z=", snoutZ, "  source vertex ", src];

shell[prims_, lbl_] := Graphics3D[prims, Boxed -> False, ViewPoint -> vp,
   ViewVertical -> vv, ImageSize -> 460, Lighting -> "Neutral",
   SphericalRegion -> True, PlotLabel -> Style[lbl, 12]];

(* ---- 1. mean curvature (robustly clipped against scan noise) ----------- *)
H = DDG`meanCurvature[trex]; H = Sign[Median[H]] H;
{lo, hi} = Quantile[H, {0.03, 0.97}];
cols = (ColorData["ThermometerColors"][Rescale[Clip[#, {lo, hi}], {lo, hi}]] &) /@ H;
Export[FileNameJoin[{imgDir, "trex_mean_curvature.png"}],
  shell[{EdgeForm[], GraphicsComplex[pts, Polygon[tris], VertexColors -> cols]},
    "mean curvature on a 3D scan"], ImageResolution -> 120];
Print["wrote trex_mean_curvature.png"];

(* ---- 2. heat-method geodesics from the tip of the snout ---------------- *)
phi = DDG`heatGeodesics[trex, src];
nb = 22;
gcols = (ColorData["Rainbow"][Round[# nb]/nb] &) /@ (phi/Max[phi]);
Export[FileNameJoin[{imgDir, "trex_geodesics.png"}],
  shell[{EdgeForm[], GraphicsComplex[pts, Polygon[tris], VertexColors -> gcols]},
    "geodesic distance from the snout (heat method)"], ImageResolution -> 120];
Print["wrote trex_geodesics.png  (phi max = ", phi // Max, ")"];

(* ---- 3. manifold harmonics on the coarse mesh -------------------------- *)
coarse = Import[FileNameJoin[{dataDir, "trex_coarse.obj"}], "MeshRegion"];
cpts = DDG`meshCoords[coarse]; ctris = DDG`meshTriangles[coarse];
Print["trex_coarse.obj: ", Length[cpts], " vertices"];
{vals, vecs} = DDG`laplacianSpectrum[coarse, 7];

modeFig[phi_, lbl_] := Module[{cmax = Max[Abs[phi]]},
  Graphics3D[{EdgeForm[],
     GraphicsComplex[cpts, Polygon[ctris],
       VertexColors -> Map[ColorData["BlueGreenYellow"], Rescale[phi, {-cmax, cmax}]]]},
    Boxed -> False, ViewPoint -> vp, ViewVertical -> vv, ImageSize -> 250,
    SphericalRegion -> True, Lighting -> "Neutral", PlotLabel -> Style[lbl, 11]]];

gallery = Table[modeFig[vecs[[j]],
    "\[Phi]" <> ToString[j - 1] <> "  \[Lambda]=" <>
      ToString[NumberForm[vals[[j]], {4, 2}]]], {j, 2, 7}];
Export[FileNameJoin[{imgDir, "trex_eigenfunctions.png"}],
  Grid[Partition[gallery, 3], Spacings -> {0, 0}], ImageResolution -> 110];
Print["wrote trex_eigenfunctions.png"];
