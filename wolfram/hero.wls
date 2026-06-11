#!/usr/bin/env wolframscript
(* hero.wls — render the candidate HERO animations for the project.

   The "point" of the whole project: ONE sparse matrix, the cotangent
   Laplacian, drives every piece of differential geometry on a mesh.
   The hero animation must make that visible. This script renders the
   candidate concepts in every candidate colour scheme so the user can
   choose from real renders:

     concepts :  lenses  bloom  flow  wave
     palettes :  aurora  sunset  viridis  bwr

   Output: docs/images/hero/hero_<concept>_<palette>.gif  (16 looping GIFs)
           docs/images/hero/hero_montage.png              (overview grid)

   Pass "final <concept> <palette>" as arguments to render ONE combination
   at full hero quality instead (bigger, more frames, finer mesh).         *)

Needs["DDG`", FileNameJoin[{DirectoryName[$InputFileName], "ddg.wl"}]];

outDir = FileNameJoin[{ParentDirectory[DirectoryName[$InputFileName]], "docs", "images", "hero"}];
If[! DirectoryQ[outDir], CreateDirectory[outDir]];

(* ----- mode: preview (all 16) or final (one, high quality) -------------- *)
argv     = Rest[$ScriptCommandLine];
finalQ   = Length[argv] >= 1 && argv[[1]] == "final";
oneConc  = If[finalQ && Length[argv] >= 2, argv[[2]], None];
onePal   = If[finalQ && Length[argv] >= 3, argv[[3]], None];

nsub   = If[finalQ, 4, 3];        (* icosphere subdivisions: 4=2562v, 3=642v *)
imgW   = If[finalQ, 720, 360];
nF     = If[finalQ, 60, 26];      (* frames per loop *)
dur    = If[finalQ, 0.05, 0.07];

Print["mode: ", If[finalQ, "FINAL " <> oneConc <> " / " <> onePal, "PREVIEW (16)"]];
Print["mesh: icosphere(", nsub, "), imgW=", imgW, ", frames=", nF];

(* ===================================================================== *)
(* 1.  THE HERO SHAPE — a gently bumpy sphere, closed and organic.        *)
(* ===================================================================== *)
sph  = DDG`icosphereMesh[nsub];
P0   = DDG`meshCoords[sph];
T    = DDG`meshTriangles[sph];
bump[p_] := (1. + 0.18 p[[3]]^2 + 0.13 Sin[2 p[[1]]] Cos[2 p[[2]]]
             + 0.07 Cos[3 p[[2]]]) Normalize[p];
X0   = bump /@ P0;
base = DDG`triMesh[X0, T];
nV   = Length[X0];
Print["hero shape: ", nV, " vertices, ", Length[T], " triangles"];

(* ===================================================================== *)
(* 2.  THE FOUR LENSES (computed once — palette-independent fields)       *)
(* ===================================================================== *)
(* curvature *)
Kf = DDG`gaussianCurvature[base];

(* a Laplace-Beltrami eigenfunction (and a few for the "bloom" concept) *)
{eVals, eVecs} = DDG`laplacianSpectrum[base, 9];
Ef    = eVecs[[5]];                         (* one lobed mode for the montage *)
modes = eVecs[[2 ;; 8]];                     (* φ1..φ7 for the bloom *)
Print["LBO eigenvalues: ", NumberForm[eVals[[1 ;; 6]], 3]];

(* heat-method geodesic distance from the top vertex *)
gsrc = First@Ordering[X0[[All, 3]], -1];
Gf   = DDG`heatGeodesics[base, gsrc];

(* mean-curvature flow stages (volume-normalised so it doesn't shrink away) *)
fr   = DDG`implicitFairing[X0, T, 5.*^-3, 18];
vol[x_] := Abs[Total[(x[[#[[1]]]] . Cross[x[[#[[2]]]], x[[#[[3]]]]]) & /@ T]]/6.;
v0   = vol[X0];
renorm[x_] := x ((v0/vol[x])^(1./3.));
stageIdx = Round[Subdivide[0, Length[fr] - 1, 9]];   (* 10 evenly-spaced stages *)
stageX   = renorm[fr[[# + 1]]] & /@ stageIdx;
stageH   = (Module[{h = DDG`meanCurvature[DDG`triMesh[#, T]]}, Sign[Mean[h]] h] &) /@ stageX;
nStage   = Length[stageX];
Print["flow: ", nStage, " stages, |H| max ", NumberForm[Max[Abs[stageH[[1]]]], 3],
      " -> ", NumberForm[Max[Abs[stageH[[-1]]]], 3]];

(* ===================================================================== *)
(* 3.  PALETTES — each a colour function [0,1]->RGB plus a background      *)
(* ===================================================================== *)
mkPal[stops_] := Function[t, Blend[stops, Clip[t, {0, 1}]]];
palettes = <|
  "aurora" -> <|"f" -> mkPal[{{0, RGBColor[0.043, 0.063, 0.149]},
        {0.28, RGBColor[0.114, 0.914, 0.714]}, {0.55, RGBColor[0.361, 0.420, 0.753]},
        {0.78, RGBColor[0.847, 0.106, 0.604]}, {1, RGBColor[1., 0.820, 0.400]}}],
     "bg" -> RGBColor[0.027, 0.035, 0.071], "label" -> "Aurora"|>,
  "sunset" -> <|"f" -> mkPal[{{0, RGBColor[0.176, 0.106, 0.306]},
        {0.4, RGBColor[0.753, 0.224, 0.169]}, {0.72, RGBColor[0.902, 0.494, 0.133]},
        {1, RGBColor[0.945, 0.769, 0.059]}}],
     "bg" -> RGBColor[0.086, 0.059, 0.141], "label" -> "Sunset"|>,
  "viridis" -> <|"f" -> mkPal[{{0, RGBColor[0.267, 0.004, 0.329]},
        {0.3, RGBColor[0.231, 0.322, 0.545]}, {0.55, RGBColor[0.129, 0.569, 0.549]},
        {0.78, RGBColor[0.369, 0.788, 0.384]}, {1, RGBColor[0.992, 0.906, 0.145]}}],
     "bg" -> RGBColor[0.051, 0.043, 0.102], "label" -> "Viridis"|>,
  "bwr" -> <|"f" -> mkPal[{{0, RGBColor[0.129, 0.400, 0.675]},
        {0.5, RGBColor[0.969, 0.969, 0.969]}, {1, RGBColor[0.698, 0.094, 0.169]}}],
     "bg" -> RGBColor[0.10, 0.11, 0.13], "label" -> "Blue-White-Red"|>|>;

(* colour a field: "seq" = sequential rescale, "div" = symmetric about 0 *)
cols["seq", field_, cf_] := cf /@ Rescale[field];
cols["div", field_, cf_] := With[{m = Max[Abs[field]] + 1.*^-9},
   cf /@ Rescale[field, {-m, m}]];
blendCols[ca_, cb_, w_] := MapThread[Blend[{#1, #2}, w] &, {ca, cb}];

(* turn a frame-spec into vertex colours under a palette. A spec is
   {coords, {{kind, field, weight}..}} — weights blend two colourings. *)
(* Fold over the (possibly empty) tail blends a crossfade onto the base
   colouring; with a single contribution it just returns the base. *)
specCols[{_, contribs_}, cf_] := Fold[
   blendCols[#1, cols[#2[[1]], #2[[2]], cf], #2[[3]]] &,
   cols[contribs[[1, 1]], contribs[[1, 2]], cf],
   Rest[contribs]];

(* ===================================================================== *)
(* 4.  FRAME SPECS PER CONCEPT (palette-independent geometry + fields)    *)
(* ===================================================================== *)
stageAt[q_] := stageX[[Clip[Round[1 + q (nStage - 1)], {1, nStage}]]];
stageHAt[q_] := stageH[[Clip[Round[1 + q (nStage - 1)], {1, nStage}]]];

(* -- concept "lenses": curvature -> eigen -> geodesic -> flow & back ---- *)
lensesSpec[i_] := Module[{u = i/nF, a = 0.22, b = 0.44, c = 0.66, d = 0.90, hw = 0.05, q},
  Which[
   u < a,  {X0, {{"div", Kf, 1}, If[u > a - hw, {"div", Ef, (u - (a - hw))/hw}, Nothing]}},
   u < b,  {X0, {{"div", Ef, 1}, If[u > b - hw, {"seq", Gf, (u - (b - hw))/hw}, Nothing]}},
   u < c,  {X0, {{"seq", Gf, 1}, If[u > c - hw, {"div", stageH[[1]], (u - (c - hw))/hw}, Nothing]}},
   u < d,  (q = (u - c)/(d - c); {stageAt[q], {{"div", stageHAt[q], 1}}}),
   True,   (q = (u - d)/(1 - d); {stageAt[1 - q], {{"div", stageHAt[1 - q], 1},
              {"div", Kf, q}}})]];

(* -- concept "bloom": eigenfunctions φ1..φ7 bloom one after another ----- *)
nMode = Length[modes];
bloomSpec[i_] := Module[{u = i nMode/nF, m, frac},
  m = Floor[u]; frac = u - m;
  m = Mod[m, nMode];
  {X0, If[frac > 0.78,
     {{"div", modes[[m + 1]], 1}, {"div", modes[[Mod[m + 1, nMode] + 1]], (frac - 0.78)/0.22}},
     {{"div", modes[[m + 1]], 1}}]}];

(* -- concept "flow": bumpy -> smooth and back (ping-pong) --------------- *)
flowSpec[i_] := Module[{u = i/nF, q},
  q = If[u < 0.5, 2 u, 2 (1 - u)];
  {stageAt[q], {{"div", stageHAt[q], 1}}}];

(* -- concept "wave": geodesic wavefronts ripple out from the source ----- *)
gMax = Max[Gf];
waveSpec[i_] := Module[{phase = 2 Pi (3 i/nF)},
  {X0, {{"seq", 0.5 + 0.5 Sin[7 Pi Gf/gMax - phase], 1}}}];

specFun = <|"lenses" -> lensesSpec, "bloom" -> bloomSpec,
            "flow" -> flowSpec, "wave" -> waveSpec|>;
concLabel = <|"lenses" -> "Four lenses, one shape", "bloom" -> "Eigenfunction bloom",
            "flow" -> "Mean-curvature flow", "wave" -> "Geodesic wavefront"|>;

(* ===================================================================== *)
(* 5.  RENDER                                                             *)
(* ===================================================================== *)
theta[i_] := 2 Pi i/nF;
g3d[coords_, vc_, i_, bg_] := Graphics3D[
   {EdgeForm[], GraphicsComplex[coords, Polygon[T], VertexColors -> vc]},
   Boxed -> False, Lighting -> "Neutral", SphericalRegion -> True,
   Background -> bg, ImageSize -> {imgW, imgW}, PlotRangePadding -> None,
   ViewPoint -> {2.7 Cos[theta[i]], 2.7 Sin[theta[i]], 1.15}, ViewVertical -> {0, 0, 1}];

frames[conc_, pal_] := Module[{cf = palettes[pal, "f"], bg = palettes[pal, "bg"], sf = specFun[conc]},
  Table[Module[{spec = sf[i]}, g3d[spec[[1]], specCols[spec, cf], i, bg]], {i, 0, nF - 1}]];

renderGif[conc_, pal_] := Module[{fs, file},
  file = FileNameJoin[{outDir, "hero_" <> conc <> "_" <> pal <> ".gif"}];
  fs = frames[conc, pal];
  Export[file, fs, "DisplayDurations" -> dur, AnimationRepetitions -> Infinity];
  Print["  wrote ", FileNameTake[file], "  (", FileByteCount[file], " B)"];
  fs[[Quotient[nF, 4]]]];   (* return a representative still *)

If[finalQ,
  (* ---- single high-quality render ---- *)
  renderGif[oneConc, onePal];
  Export[FileNameJoin[{outDir, "hero_" <> oneConc <> "_" <> onePal <> "_still.png"}],
     frames[oneConc, onePal][[Quotient[nF, 4]]]];
  Print["final hero done."],

  (* ---- all 16 + montage ---- *)
  Module[{stills = <||>, concepts = {"lenses", "bloom", "flow", "wave"},
          pals = {"aurora", "sunset", "viridis", "bwr"}, grid},
   Do[Print["[", c, " / ", p, "]"]; stills[{c, p}] = renderGif[c, p],
     {c, concepts}, {p, pals}];
   grid = Grid[
     Prepend[
       Table[Prepend[Table[Rasterize[stills[{c, p}], ImageSize -> 150], {p, pals}],
          Rotate[Style[concLabel[c], 11, Bold], Pi/2]], {c, concepts}],
       Prepend[Style[palettes[#, "label"], 11, Bold] & /@ pals, ""]],
     Frame -> All, Background -> GrayLevel[0.12], FrameStyle -> GrayLevel[0.3],
     Spacings -> {0.6, 0.6}];
   Export[FileNameJoin[{outDir, "hero_montage.png"}], grid, ImageResolution -> 120];
   Print["wrote hero_montage.png"]]];

Print["done -> ", outDir];
