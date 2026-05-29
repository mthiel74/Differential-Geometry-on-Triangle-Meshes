#!/usr/bin/env wolframscript
(* yoda.wls — the DDG operators on a real 3D scan.

   A 12k-triangle decimation of a scanned "Yoda" figurine (see
   scripts/prepare_yoda.py) stands in for the textbook sphere/torus, showing
   that the same operators run on messy real geometry:

   1. discrete mean curvature  -> docs/images/yoda_mean_curvature.png
   2. heat-method geodesic distance from the top of the head
                                 -> docs/images/yoda_geodesics.png
   3. Laplace-Beltrami eigenfunctions (manifold harmonics) on the coarse mesh
                                 -> docs/images/yoda_eigenfunctions.png        *)

Needs["DDG`", FileNameJoin[{DirectoryName[$InputFileName], "ddg.wl"}]];

dataDir = FileNameJoin[{ParentDirectory[DirectoryName[$InputFileName]], "data"}];
imgDir  = FileNameJoin[{ParentDirectory[DirectoryName[$InputFileName]], "docs", "images"}];

vp = {0.45, -0.4, 2.3}; vv = {0, 1, 0};       (* Yoda stands along +y *)

yoda = Import[FileNameJoin[{dataDir, "yoda.obj"}], "MeshRegion"];
pts = DDG`meshCoords[yoda]; tris = DDG`meshTriangles[yoda];
Print["yoda.obj: ", Length[pts], " vertices, ", Length[tris], " triangles"];

shell[prims_, lbl_] := Graphics3D[prims, Boxed -> False, ViewPoint -> vp,
   ViewVertical -> vv, ImageSize -> 460, Lighting -> "Neutral",
   PlotLabel -> Style[lbl, 12]];

(* ---- 1. mean curvature (robustly clipped against scan noise) ----------- *)
H = DDG`meanCurvature[yoda]; H = Sign[Median[H]] H;
{lo, hi} = Quantile[H, {0.03, 0.97}];
cols = (ColorData["ThermometerColors"][Rescale[Clip[#, {lo, hi}], {lo, hi}]] &) /@ H;
Export[FileNameJoin[{imgDir, "yoda_mean_curvature.png"}],
  shell[{EdgeForm[], GraphicsComplex[pts, Polygon[tris], VertexColors -> cols]},
    "mean curvature on a 3D scan"], ImageResolution -> 120];
Print["wrote yoda_mean_curvature.png"];

(* ---- 2. heat-method geodesics from the top of the head ----------------- *)
src = First@Ordering[pts[[All, 2]], -1];       (* highest point = head *)
phi = DDG`heatGeodesics[yoda, src];
nb = 22;
gcols = (ColorData["Rainbow"][Round[# nb]/nb] &) /@ (phi/Max[phi]);
Export[FileNameJoin[{imgDir, "yoda_geodesics.png"}],
  shell[{EdgeForm[], GraphicsComplex[pts, Polygon[tris], VertexColors -> gcols]},
    "geodesic distance from the head (heat method)"], ImageResolution -> 120];
Print["wrote yoda_geodesics.png  (phi max = ", phi // Max, ")"];

(* ---- 3. manifold harmonics on the coarse mesh -------------------------- *)
coarse = Import[FileNameJoin[{dataDir, "yoda_coarse.obj"}], "MeshRegion"];
cpts = DDG`meshCoords[coarse]; ctris = DDG`meshTriangles[coarse];
Print["yoda_coarse.obj: ", Length[cpts], " vertices"];
{vals, vecs} = DDG`laplacianSpectrum[coarse, 7];

modeFig[phi_, lbl_] := Module[{cmax = Max[Abs[phi]]},
  Graphics3D[{EdgeForm[],
     GraphicsComplex[cpts, Polygon[ctris],
       VertexColors -> Map[ColorData["BlueGreenYellow"], Rescale[phi, {-cmax, cmax}]]]},
    Boxed -> False, ViewPoint -> vp, ViewVertical -> vv, ImageSize -> 250,
    Lighting -> "Neutral", PlotLabel -> Style[lbl, 11]]];

gallery = Table[modeFig[vecs[[j]],
    "\[Phi]" <> ToString[j - 1] <> "  \[Lambda]=" <>
      ToString[NumberForm[vals[[j]], {4, 2}]]], {j, 2, 7}];
Export[FileNameJoin[{imgDir, "yoda_eigenfunctions.png"}],
  Grid[Partition[gallery, 3], Spacings -> {0, 0}], ImageResolution -> 110];
Print["wrote yoda_eigenfunctions.png"];
