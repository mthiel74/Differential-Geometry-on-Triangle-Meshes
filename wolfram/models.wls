#!/usr/bin/env wolframscript
(* models.wls — export the hero figures as live Graphics3D objects (.wxf) so
   the community notebook can embed them as *rotatable* 3D graphics rather
   than static rasters. Resolutions are kept moderate to bound the notebook
   size; the standalone *.wls scripts keep their higher-resolution PNGs for
   GitHub (which cannot show interactive graphics).                          *)

Needs["DDG`", FileNameJoin[{DirectoryName[$InputFileName], "ddg.wl"}]];
Needs["GraphDDG`", FileNameJoin[{DirectoryName[$InputFileName], "graphgeometry.wl"}]];
modelDir = FileNameJoin[{ParentDirectory[DirectoryName[$InputFileName]], "docs", "models"}];
If[! DirectoryQ[modelDir], CreateDirectory[modelDir]];

save[name_, g_] := (Export[FileNameJoin[{modelDir, name <> ".wxf"}], g];
   Print["  ", name, ".wxf  ", FileByteCount[FileNameJoin[{modelDir, name <> ".wxf"}]], " B"]);

surf[pts_, tris_, cols_, opts___] := Graphics3D[
   {EdgeForm[], GraphicsComplex[pts, Polygon[tris], VertexColors -> cols]},
   Boxed -> False, Lighting -> "Neutral", SphericalRegion -> True,
   ImageSize -> 420, opts];

diverging[v_] := With[{m = Max[Abs[v]] + 1.*^-12},
   Map[ColorData["TemperatureMap"], Rescale[v, {-m, m}]]];
banded[v_, nb_] := (ColorData["Rainbow"][Round[# nb]/nb] &) /@ (v/Max[v]);

Print["exporting live models -> docs/models/"];

(* ---- 1. curvature on a torus ------------------------------------------- *)
R = 2.; r = 0.8;
tor = DDG`torusMesh[R, r, 48, 96]; tp = DDG`meshCoords[tor]; tt = DDG`meshTriangles[tor];
save["torus_K", surf[tp, tt, diverging[DDG`gaussianCurvature[tor]],
   PlotLabel -> "Gaussian curvature K (drag to rotate)"]];
hh = DDG`meanCurvature[tor]; hh = Sign[Mean[hh]] hh;
save["torus_H", surf[tp, tt, Map[ColorData["TemperatureMap"], Rescale[hh]],
   PlotLabel -> "mean curvature H (drag to rotate)"]];

(* ---- 2. a Laplace-Beltrami eigenfunction on the sphere ----------------- *)
sph = DDG`icosphereMesh[4]; sp = DDG`meshCoords[sph]; st = DDG`meshTriangles[sph];
{vals, vecs} = DDG`laplacianSpectrum[sph, 6];
save["sphere_mode", surf[sp, st,
   Map[ColorData["BlueGreenYellow"], Rescale[vecs[[5]]]],
   PlotLabel -> "an eigenfunction \[Phi]\:2084 (\[Lambda]\[TildeTilde]6, l=2)"]];

(* ---- 3. faired mesh (mean-curvature flow result) ----------------------- *)
SeedRandom[7];
bumpy = ((1. + 0.16 #[[3]]^2 + 0.12 Sin[2 #[[1]]] Cos[2 #[[2]]]) Normalize[#]) & /@ sp;
fr = DDG`implicitFairing[bumpy, st, 6.*^-3, 20];
v0 = Abs[Total[(fr[[1, #[[1]]]] . Cross[fr[[1, #[[2]]]], fr[[1, #[[3]]]]]) & /@ st]]/6.;
norm[x_] := x ((v0/(Abs[Total[(x[[#[[1]]]] . Cross[x[[#[[2]]]], x[[#[[3]]]]]) & /@ st]]/6.))^(1./3.));
save["mcf_bumpy", surf[norm[fr[[1]]], st, diverging[Sign[Mean[#]] # &@DDG`meanCurvature[DDG`triMesh[fr[[1]], st]]],
   PlotLabel -> "before flow"]];
save["mcf_faired", surf[norm[fr[[-1]]], st, diverging[Sign[Mean[#]] # &@DDG`meanCurvature[DDG`triMesh[fr[[-1]], st]]],
   PlotLabel -> "after mean-curvature flow"]];

(* ---- 4. geodesic distance fields --------------------------------------- *)
gsrc = First@Ordering[sp[[All, 3]], -1];
save["geo_sphere", surf[sp, st, banded[DDG`heatGeodesics[sph, gsrc], 16],
   PlotLabel -> "geodesic distance on a sphere"]];
tg = DDG`torusMesh[R, r, 40, 80]; tgp = DDG`meshCoords[tg]; tgt = DDG`meshTriangles[tg];
tgsrc = First@Ordering[tgp[[All, 1]], -1];
save["geo_torus", surf[tgp, tgt, banded[DDG`heatGeodesics[tg, tgsrc], 18],
   PlotLabel -> "geodesic distance on a torus"]];

(* ---- 5. harmonic field on the torus (Hodge) ---------------------------- *)
th = DDG`torusMesh[R, r, 24, 48]; thp = DDG`meshCoords[th]; tht = DDG`meshTriangles[th];
edges = DDG`decOperators[th]["edges"];
field[p_] := {-p[[2]], p[[1]], 0.} + 0.4 {0., 0., Sin[3 p[[1]]]};
omega = (field[(thp[[#[[1]]]] + thp[[#[[2]]]])/2] . (thp[[#[[2]]]] - thp[[#[[1]]]]) &) /@ edges;
harm = DDG`hodgeDecomposition[th, omega]["harmonic"];
fv = DDG`oneFormToFaceField[th, harm];
cen = Mean /@ (thp[[#]] & /@ tht);
sc = 0.4/Max[Norm /@ fv];
selT = Range[1, Length[tht], 2];
hg = Graphics3D[{
    {EdgeForm[], FaceForm[GrayLevel[0.85]], GraphicsComplex[thp, Polygon[tht]]},
    {RGBColor[0.1, 0.55, 0.2], Thickness[0.003],
     Table[With[{c = cen[[i]], v = sc fv[[i]]}, {Arrowheads[0.22 Norm[v]],
        Arrow[{c, c + v}]}], {i, selT}]}},
   Boxed -> False, Lighting -> "Neutral", SphericalRegion -> True, ImageSize -> 460,
   PlotLabel -> "harmonic field \[Gamma] (circulates the handle)"];
save["hodge_harmonic", hg];

(* ---- 6. the 3D scan: Yoda ---------------------------------------------- *)
yoda = Import[FileNameJoin[{ParentDirectory[DirectoryName[$InputFileName]], "data", "yoda.obj"}], "MeshRegion"];
yp = DDG`meshCoords[yoda]; yt = DDG`meshTriangles[yoda];
yview = {ViewPoint -> {0.45, -0.4, 2.3}, ViewVertical -> {0, 1, 0}};
ysrc = First@Ordering[yp[[All, 2]], -1];
save["yoda_geo", surf[yp, yt, banded[DDG`heatGeodesics[yoda, ysrc], 22],
   Sequence @@ yview, PlotLabel -> "geodesic distance from the head"]];
yh = DDG`meanCurvature[yoda]; yh = Sign[Median[yh]] yh;
{ylo, yhi} = Quantile[yh, {0.03, 0.97}];
save["yoda_H", surf[yp, yt,
   Map[ColorData["ThermometerColors"], Rescale[Clip[yh, {ylo, yhi}], {ylo, yhi}]],
   Sequence @@ yview, PlotLabel -> "mean curvature on the scan"]];

(* ---- 7. graph geometry: the sphere mesh-graph "soccer ball" defects ---- *)
ico = DDG`icosphereMesh[3]; ipp = DDG`meshCoords[ico]; itt = DDG`meshTriangles[ico];
deg5 = Flatten@Position[VertexDegree[GraphDDG`meshGraph[ico]], 5];
save["soccer", Graphics3D[{
    {EdgeForm[GrayLevel[0.45]], FaceForm[GrayLevel[0.92]], GraphicsComplex[ipp, Polygon[itt]]},
    {RGBColor[0.85, 0.15, 0.15], Sphere[ipp[[#]], 0.05] & /@ deg5}},
   Boxed -> False, Lighting -> "Neutral", SphericalRegion -> True, ImageSize -> 420,
   PlotLabel -> "12 curvature defects on a hexagonal sphere"]];

Print["done."];
