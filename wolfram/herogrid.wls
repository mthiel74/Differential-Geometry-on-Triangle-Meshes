#!/usr/bin/env wolframscript
(* herogrid.wls — the HERO: a single 2x2 grid animation, aurora palette,
   showing the four faces of the cotangent Laplacian on ONE rotating torus:

        Curvature              Spectrum (eigenmodes)
        Mean-curvature flow    Geodesic distance

   The torus is the same object in every panel — the point being that one
   sparse matrix produces all four geometries. Output:

        docs/images/hero/hero.gif   (+ hero_poster.png still)

   Pass "draft" as an argument for a fast low-res pass while iterating.     *)

Needs["DDG`", FileNameJoin[{DirectoryName[$InputFileName], "ddg.wl"}]];
outDir = FileNameJoin[{ParentDirectory[DirectoryName[$InputFileName]], "docs", "images", "hero"}];
If[! DirectoryQ[outDir], CreateDirectory[outDir]];

argv  = Rest[$ScriptCommandLine];
draft = MemberQ[argv, "draft"];
nF    = If[draft, 16, 30];          (* frames in the loop *)
pw    = If[draft, 240, 360];        (* panel pixel size *)
nu    = If[draft, 24, 36];          (* torus tube resolution *)
nv    = If[draft, 48, 72];          (* torus ring resolution *)
ncol  = If[draft, 64, 72];          (* GIF colour depth *)
Print["herogrid: frames=", nF, " panel=", pw, " torus=", nu, "x", nv];

(* ===================== the shape: one clean torus ====================== *)
R = 2.3; r = 0.92;
tor = DDG`torusMesh[R, r, nu, nv];
X0  = DDG`meshCoords[tor];
T   = DDG`meshTriangles[tor];
nVtx = Length[X0];
Print["torus: ", nVtx, " vertices, ", Length[T], " triangles"];

(* ===================== the four fields ================================= *)
(* curvature *)
Kf = DDG`gaussianCurvature[tor];

(* Laplace-Beltrami eigenmodes for the blooming "spectrum" panel *)
{eVals, eVecs} = DDG`laplacianSpectrum[tor, 12];
modes = eVecs[[2 ;; 10]];            (* skip the constant phi0 *)
nMode = Length[modes];
Print["eigenvalues: ", NumberForm[eVals[[1 ;; 6]], 3]];

(* heat-method geodesic distance from one vertex (for the wavefront) *)
gsrc = First@Ordering[X0[[All, 1]], -1];
Gf   = DDG`heatGeodesics[tor, gsrc];
gMax = Max[Gf];

(* mean-curvature flow: start from a BUMPY torus and fair it smooth.
   Volume-renormalised so it denoises in place instead of shrinking away. *)
tubeN[p_] := Normalize[p - R Normalize[{p[[1]], p[[2]], 0.}]];
uOf = DDG`torusAngleU[X0, R, r];
vOf = ArcTan[X0[[All, 1]], X0[[All, 2]]];
bumpAmp = MapThread[0.16 Sin[5 #2] Cos[4 #1] + 0.09 Sin[9 #1] &, {uOf, vOf}];
Xbump = MapThread[#1 + #2 tubeN[#1] &, {X0, bumpAmp}];
fr  = DDG`implicitFairing[Xbump, T, 3.*^-3, 12];
vol[x_] := Abs[Total[(x[[#[[1]]]] . Cross[x[[#[[2]]]], x[[#[[3]]]]]) & /@ T]]/6.;
v0  = vol[Xbump];
renorm[x_] := x ((v0/vol[x])^(1./3.));
sIdx = Round[Subdivide[0, Length[fr] - 1, 9]];
stageX = renorm[fr[[# + 1]]] & /@ sIdx;
stageH = (Module[{h = DDG`meanCurvature[DDG`triMesh[#, T]]}, Sign[Mean[h]] h] &) /@ stageX;
nStage = Length[stageX];

(* ===================== aurora palette + studio look ==================== *)
aurora = Function[t, Blend[{
    {0.00, RGBColor[0.02, 0.03, 0.08]}, {0.22, RGBColor[0.122, 0.878, 0.659]},
    {0.45, RGBColor[0.165, 0.616, 0.957]}, {0.68, RGBColor[0.482, 0.424, 0.965]},
    {0.88, RGBColor[0.910, 0.294, 0.788]}, {1.00, RGBColor[1., 0.827, 0.431]}},
   Clip[t, {0, 1}]]];
bg = RGBColor[0.02, 0.025, 0.05];
seq[f_] := aurora /@ Rescale[f];
div[f_] := With[{m = Max[Abs[f]] + 1.*^-9}, aurora /@ Rescale[f, {-m, m}]];

lights = {{"Ambient", GrayLevel[0.5]},
   {"Point", GrayLevel[0.75], {5, -4, 6}}, {"Point", GrayLevel[0.45], {-6, 4, 2}},
   {"Point", GrayLevel[0.3], {0, 0, -7}}};
theta[i_] := 2 Pi i/nF;

panel[coords_, vc_, i_, lbl_] := Graphics3D[
   {Specularity[GrayLevel[0.55], 22], EdgeForm[],
    GraphicsComplex[coords, Polygon[T], VertexColors -> vc]},
   Boxed -> False, Lighting -> lights, SphericalRegion -> True, Background -> bg,
   ImageSize -> {pw, pw}, PlotRangePadding -> None, Method -> {"ShrinkWrap" -> True},
   ViewPoint -> {3.0 Cos[theta[i]], 3.0 Sin[theta[i]], 1.55}, ViewVertical -> {0, 0, 1},
   PlotLabel -> Style[lbl, RGBColor[0.85, 0.88, 0.95], 13, FontFamily -> "Helvetica"]];

(* ---- the four animated panels at frame i ---- *)
pCurv[i_] := panel[X0, div[Kf], i, "curvature  K"];
pSpec[i_] := Module[{u = i nMode/nF, m, frac, vc},
   m = Mod[Floor[u], nMode]; frac = u - Floor[u];
   vc = If[frac > 0.78,
       MapThread[Blend[{#1, #2}, (frac - 0.78)/0.22] &,
          {div[modes[[m + 1]]], div[modes[[Mod[m + 1, nMode] + 1]]]}],
       div[modes[[m + 1]]]];
   panel[X0, vc, i, "spectrum  \[Phi]\[Lambda]"]];
pFlow[i_] := Module[{u = i/nF, q, k},
   q = If[u < 0.5, 2 u, 2 (1 - u)];
   k = Clip[Round[1 + q (nStage - 1)], {1, nStage}];
   panel[stageX[[k]], div[stageH[[k]]], i, "mean-curvature flow"]];
pGeo[i_] := panel[X0, seq[0.5 + 0.5 Sin[7 Pi Gf/gMax - 2 Pi (3 i/nF)]], i, "geodesic distance"];

(* ===================== assemble the grid frames ======================= *)
title = Rasterize[
   Style["One operator \[LongDash] the cotangent Laplacian \[LongDash] is all of this",
     RGBColor[0.92, 0.94, 0.98], 16, FontFamily -> "Helvetica", FontWeight -> Bold],
   Background -> bg, ImageResolution -> 144];

frameImg[i_] := Module[{grid},
   grid = ImageAssemble[{{Rasterize[pCurv[i]], Rasterize[pSpec[i]]},
                         {Rasterize[pFlow[i]], Rasterize[pGeo[i]]}},
      Background -> bg, "Spacings" -> 10];
   ImageAssemble[{{ImageResize[title, {ImageDimensions[grid][[1]]}]}, {grid}},
      Background -> bg, "Spacings" -> 6]];

Print["rendering ", nF, " grid frames ..."];
frames = Table[If[Mod[i, 6] == 0, Print["  frame ", i, "/", nF]]; frameImg[i], {i, 0, nF - 1}];
frames = ColorQuantize[#, ncol] & /@ frames;

gif = FileNameJoin[{outDir, "hero.gif"}];
Export[gif, frames, "DisplayDurations" -> If[draft, 0.09, 0.07], AnimationRepetitions -> Infinity];
Export[FileNameJoin[{outDir, "hero_poster.png"}], frames[[Quotient[nF, 4]]]];
Print["wrote ", FileNameTake[gif], "  (", Round[FileByteCount[gif]/1024], " KB), ",
   "poster + done -> ", outDir];
