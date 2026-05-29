#!/usr/bin/env wolframscript
(* meanflow.wls — mean-curvature flow (implicit fairing).

   Flow  dx/dt = Delta x = M^-1 L x  smooths a surface by its mean curvature.
   Explicit Euler is stable only for tiny tau; the semi-implicit step
   (M - tau L) x' = M x  (Desbrun-Meyer-Schroder-Barr 1999) is stable for any
   tau and is what DDG`implicitFairing uses.

   1. Quantitative check: under this flow a round sphere of radius R obeys
      d(R^2)/dt = -4  (since Delta x = -2H n = -(2/R) n on a unit-speed sphere),
      i.e. R(t)^2 = R0^2 - 4 t.  We recover the slope -4.
   2. Visual: a bumpy sphere relaxes to round -> animated GIF + before/after.  *)

Needs["DDG`", FileNameJoin[{DirectoryName[$InputFileName], "ddg.wl"}]];

sci[x_] := If[x == 0., "0",
  With[{e = Floor[Log10[Abs[x]]]},
    ToString[NumberForm[N[x]/10.^e, {4, 3}]] <> "e" <> ToString[e]]];

imgDir = FileNameJoin[{ParentDirectory[DirectoryName[$InputFileName]], "docs", "images"}];
meanRadius[x_] := Mean[Norm /@ x];

(* ---- 1. shrinking-sphere rate check ------------------------------------ *)
base = DDG`icosphereMesh[4];
tris = DDG`meshTriangles[base];
R0 = 1.0;
tau = 2.*^-3;
nstep = 12;
frames = DDG`implicitFairing[R0 DDG`meshCoords[base], tris, tau, nstep];
radii = meanRadius /@ frames;
ts = tau Range[0, nstep];
(* least-squares slope of R^2 vs t (closed form; should be approx -4) *)
lsSlope[xs_, ys_] := With[{n = Length[xs]},
  (n Total[xs ys] - Total[xs] Total[ys])/(n Total[xs^2] - Total[xs]^2)];
slope = lsSlope[ts, radii^2];
Print["mean-curvature flow on a unit sphere:"];
Print["  R(0) = ", sci[radii[[1]]], "   R(end) = ", sci[radii[[-1]]]];
Print["  fitted d(R^2)/dt = ", sci[slope], "   (theory = -4)   rel err = ",
  sci[Abs[(slope + 4)/4]]];

(* ---- 2. bumpy sphere relaxes to round ---------------------------------- *)
SeedRandom[42];
pts0 = DDG`meshCoords[DDG`icosphereMesh[4]];
(* smooth, fairly symmetric low-frequency radial perturbation + a little
   noise. Kept gentle so the flow does not degrade triangle quality. *)
bump[p_] := 1. + 0.16 p[[3]]^2 + 0.12 Sin[2 p[[1]]] Cos[2 p[[2]]]
   + 0.03 RandomReal[{-1, 1}];
bumpy = (bump[#] Normalize[#]) & /@ pts0;

tauB = 6.*^-3; nB = 20;
framesB = DDG`implicitFairing[bumpy, tris, tauB, nB];
minArea[x_] := Min[(0.5 Norm[Cross[x[[#[[2]]]] - x[[#[[1]]]],
     x[[#[[3]]]] - x[[#[[1]]]]]]) & /@ tris];
Print["  triangle-quality guard: min area ", sci[minArea[framesB[[1]]]],
  " -> ", sci[minArea[framesB[[-1]]]]];

(* sphericity = isoperimetric ratio 36 pi V^2 / A^3 (1 = perfect sphere).
   Enclosed volume via the divergence theorem: V = (1/6) sum_t v0.(v1 x v2). *)
enclosedVolume[x_] := Abs[Total[
   (x[[#[[1]]]] . Cross[x[[#[[2]]]], x[[#[[3]]]]]) & /@ tris]]/6.;
roundness[x_] := 36. Pi enclosedVolume[x]^2/Area[DDG`triMesh[x, tris]]^3;
Print["bumpy-sphere fairing:  roundness ",
  sci[roundness[framesB[[1]]]], " -> ", sci[roundness[framesB[[-1]]]],
  "   (1 = perfect sphere)"];

(* MCF shrinks volume; for the *fairing* visual rescale every frame back to
   the initial enclosed volume so the eye sees smoothing, not shrinking. *)
v0 = enclosedVolume[framesB[[1]]];
framesBn = (#*(v0/enclosedVolume[#])^(1./3.)) & /@ framesB;

(* fixed colour scale (from the initial bumpy frame) so the animation reads
   as mean curvature *equalising*; TemperatureMap has no black endpoint. *)
hrange = MinMax[Module[{m = DDG`triMesh[framesBn[[1]], tris], h},
    h = DDG`meanCurvature[m]; Sign[Mean[h]] h]];

renderFrame[x_, label_] := Module[{H, mr, cols},
  mr = DDG`triMesh[x, tris];
  H = DDG`meanCurvature[mr]; H = Sign[Mean[H]] H;
  cols = (ColorData["TemperatureMap"][Rescale[#, hrange]] &) /@ H;
  Graphics3D[{EdgeForm[],
     GraphicsComplex[x, Polygon[tris], VertexColors -> cols]},
    Boxed -> False, SphericalRegion -> True, ViewPoint -> {1.6, -2.4, 1.4},
    ImageSize -> 520, Lighting -> "Neutral",
    PlotRange -> 1.4 {{-1, 1}, {-1, 1}, {-1, 1}},
    PlotLabel -> Style[label, 12]]];

ba = FileNameJoin[{imgDir, "mcf_before_after.png"}];
Export[ba,
  GraphicsRow[{renderFrame[framesBn[[1]], "initial (bumpy)"],
     renderFrame[framesBn[[-1]], "after fairing (volume-preserved)"]},
    Spacings -> 0, ImageSize -> 1040], ImageResolution -> 120];
Print["wrote ", ba];

(* animated GIF — subsample frames, quantise to keep the file small *)
sel = framesBn[[1 ;; -1 ;; 2]];
gif = FileNameJoin[{imgDir, "mcf_flow.gif"}];
Export[gif,
  Table[ColorQuantize[
     ImageResize[Rasterize[renderFrame[sel[[k]],
        "mean-curvature flow"], ImageSize -> 480], {360}], 128],
    {k, Length[sel]}],
  "DisplayDurations" -> 0.18, AnimationRepetitions -> Infinity];
Print["wrote ", gif, " (", FileByteCount[gif], " B)"];
