#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  Build community/ddg.nb — the educational long-form notebook for the
    Wolfram Community submission on discrete differential geometry on
    triangle meshes.

    Architecture (mirrors the ENSO-emergence builder): assemble a flat list
    of fully-formed Cell expressions, then persist via an invisible
    FrontEnd document + NotebookSave (which serialises Image / AnimatedImage
    rasters correctly) and Export the PDF.

    Sections
    --------
      Abstract
      1. Two curvatures from one mesh (Gaussian + mean; Gauss-Bonnet)
      2. The cotangent Laplace-Beltrami operator
      3. Spectral geometry: hearing the shape of a mesh
      4. Mean-curvature flow: smoothing as diffusion
      5. Outlook: geodesics and discrete exterior calculus
      6. References
      7. Reproducibility
*)

repoRoot = ParentDirectory@DirectoryName[$InputFileName];
imgDir   = FileNameJoin[{repoRoot, "docs", "images"}];
outNb    = FileNameJoin[{repoRoot, "community", "ddg.nb"}];
outPdf   = FileNameJoin[{repoRoot, "community", "ddg.pdf"}];

(* ---- cell helpers ------------------------------------------------------ *)
title[t_]    := Cell[t, "Title"];
subtitle[t_] := Cell[t, "Subtitle"];
hd1[t_]      := Cell[t, "Section"];
hd2[t_]      := Cell[t, "Subsection"];

para[stuff_] := Cell[TextData[stuff], "Text", CellMargins -> {{50, 50}, {6, 6}}];
abstract[stuff_] := Cell[TextData[stuff], "Text", FontSize -> 14,
   CellMargins -> {{60, 60}, {8, 8}}, Background -> RGBColor[0.97, 0.97, 0.93]];

bold[s_] := StyleBox[s, FontWeight -> Bold];
ital[s_] := StyleBox[s, FontSlant -> Italic];
mono[s_] := StyleBox[s, FontFamily -> "Courier"];
link[txt_, url_] := ButtonBox[StyleBox[txt, FontColor -> RGBColor[0.2, 0.3, 0.8]],
   BaseStyle -> "Hyperlink",
   ButtonData -> {URL[url], None}, ButtonNote -> url];

dm[expr_] := Cell[BoxData[ToBoxes[TraditionalForm[expr]]], "DisplayFormula",
   CellMargins -> {{70, Automatic}, {8, 8}}, TextAlignment -> Center];

captionCell[args__] := Cell[TextData[{args}], "Text", FontSlant -> Italic,
   FontSize -> 11, FontColor -> GrayLevel[0.35], TextAlignment -> Center,
   CellMargins -> {{70, 70}, {2, 14}}];

wlIn[code_String] := Cell[code, "Input", CellMargins -> {{60, 30}, {6, 6}}, FontSize -> 11];
codeIn[code_String] := Cell[code, "Program", CellMargins -> {{60, 30}, {6, 6}},
   FontFamily -> "Courier", FontSize -> 11, ShowStringCharacters -> False,
   Background -> RGBColor[0.95, 0.96, 0.98]];

imgCell[file_, width_: 580] := With[{p = FileNameJoin[{imgDir, file}]},
   If[FileExistsQ[p],
     Cell[BoxData[ToBoxes[Image[Import[p], ImageSize -> width]]], "Output",
       ShowCellLabel -> False, TextAlignment -> Center,
       CellMargins -> {{Automatic, Automatic}, {10, 6}}],
     Cell[TextData[{bold["[ missing figure: " <> file <> " ]"]}], "Text",
       FontColor -> RGBColor[0.7, 0.1, 0.1], TextAlignment -> Center]]];

(* GIF -> AnimatedImage so the Wolfram Community web renderer plays it
   (it drops Video[] but keeps AnimatedImage as a looping <img>). *)
animCell[file_, width_: 460] := With[{p = FileNameJoin[{imgDir, file}]},
   If[FileExistsQ[p],
     Module[{frames = Import[p, {"GIF", "ImageList"}]},
       frames = ColorQuantize[ImageResize[#, {width}], 128] & /@ frames;
       Cell[BoxData[ToBoxes[AnimatedImage[frames, FrameRate -> 6,
            AnimationRepetitions -> Infinity]]], "Output",
         ShowCellLabel -> False, TextAlignment -> Center,
         CellMargins -> {{Automatic, Automatic}, {10, 10}}]],
     Cell[TextData[{bold["[ missing animation: " <> file <> " ]"]}], "Text",
       FontColor -> RGBColor[0.7, 0.1, 0.1], TextAlignment -> Center]]];

ref[args__] := Cell[TextData[{args}], "Text", FontSize -> 11,
   CellMargins -> {{60, 50}, {3, 3}}];

(* ===================================================================== *)

cells = Flatten[{

title["Differential Geometry on Triangle Meshes"],
subtitle["One matrix \[LongDash] the cotangent Laplacian \[LongDash] from curvature to spectra to flow, in Wolfram Language"],

abstract[{bold["Abstract. "],
  "Smooth surfaces carry curvature, a Laplace operator, a spectrum and a notion of \
flow. A triangle mesh has none of these a priori \[LongDash] yet ", ital["discrete \
differential geometry"], " (DDG) builds operators on meshes that reproduce the \
theorems of the smooth theory exactly. This notebook develops the core of DDG in \
pure Wolfram Language and validates every quantity against a smooth reference: \
angle-defect Gaussian curvature (checked by Gauss\[Dash]Bonnet), the cotangent \
Laplace\[Dash]Beltrami operator, mean curvature, the Laplacian spectrum (which \
recovers the sphere's eigenvalues ", ital["l"], "(", ital["l"], "+1)), and \
mean-curvature flow. A single matrix \[LongDash] the cotangent Laplacian \
\[LongDash] drives all of it."}],

(* ---------------------------------------------------------------- *)
hd1["1.  Two curvatures from one mesh"],
para[{"A smooth surface has two curvatures at each point: the ", bold["Gaussian \
curvature"], " ", ital["K"], " (intrinsic \[LongDash] bending that a flat sheet \
cannot reproduce) and the ", bold["mean curvature"], " ", ital["H"], " (extrinsic \
\[LongDash] how the surface sits in space). On a triangle mesh both become \
per-vertex numbers."}],
para[{"The ", bold["angle defect"], " concentrates all Gaussian curvature at the \
vertices: for an interior vertex ", ital["i"], " with incident triangle angles ",
   StyleBox["\[Theta]", FontSlant -> Italic], ", the integrated Gaussian curvature \
is the failure of those angles to close up to a full turn,"}],
dm[HoldForm[Subscript[K, i] == 2 \[Pi] - Sum[Subscript[\[Theta], j], j]]],
para[{"Summed over a closed mesh this gives the ", bold["discrete \
Gauss\[Dash]Bonnet theorem"], ", an exact identity tying local geometry to global \
topology (", StyleBox["\[Chi]", FontSlant -> Italic], " is the Euler \
characteristic):"}],
dm[HoldForm[Sum[Subscript[K, i], i] == 2 \[Pi] \[Chi]]],
para[{"Our implementation reproduces it to machine precision: the sphere \
(", StyleBox["\[Chi]", FontSlant -> Italic], " = 2) totals 4", StyleBox["\[Pi]",
   FontSlant -> Italic], " to a residual of 10", StyleBox["\[Minus]12",
   FontSize -> 9], ", and the torus (", StyleBox["\[Chi]", FontSlant -> Italic],
   " = 0) totals zero. The pointwise curvature ", ital["K"],
   " = (angle defect)/(vertex area) matches the analytic torus curvature to a \
relative L2 error of 5\[Times]10", StyleBox["\[Minus]4", FontSize -> 9], "."}],
imgCell["torus_gaussian_curvature.png"],
captionCell["Figure 1. Discrete Gaussian curvature on a torus from the angle \
defect: positive (warm) on the outer rim, negative (blue) on the inner rim, \
summing to zero as Gauss\[Dash]Bonnet demands."],
para[{"The ", bold["mean curvature"], " comes from the cotangent Laplacian of \
\[Section]2 applied to the vertex positions (the mean-curvature normal of Meyer et \
al. 2003); it matches the smooth torus mean curvature to 3\[Times]10",
   StyleBox["\[Minus]4", FontSize -> 9], " and the unit sphere's ", ital["H"],
   " = 1 to 5\[Times]10", StyleBox["\[Minus]3", FontSize -> 9], "."}],
imgCell["torus_mean_curvature.png"],
captionCell["Figure 2. Discrete mean curvature on the torus, computed from the \
cotangent Laplacian of the coordinate functions."],

(* ---------------------------------------------------------------- *)
hd1["2.  The cotangent Laplace\[Dash]Beltrami operator"],
para[{"The workhorse of DDG is the ", bold["cotangent Laplacian"],
   ". For a function ", ital["f"], " sampled at vertices,"}],
dm[HoldForm[Subscript[(L f), i] ==
    (1/2) Sum[(Cot[Subscript[\[Alpha], ij]] + Cot[Subscript[\[Beta], ij]])
       (Subscript[f, j] - Subscript[f, i]), j]]],
para[{"where ", StyleBox["\[Alpha]", FontSlant -> Italic], " and ",
   StyleBox["\[Beta]", FontSlant -> Italic], " are the two angles opposite the \
edge (", ital["i"], ",", ital["j"], "). It is symmetric, its row sums vanish \
(constants are in its kernel), and it is negative semi-definite \[LongDash] the \
discrete analogue of ", StyleBox["\[CapitalDelta]", FontSlant -> Italic], ". \
Paired with the diagonal mass matrix ", ital["M"], " of vertex areas, it is the \
single operator behind every section that follows. In Wolfram Language it is one \
sparse matrix:"}],
wlIn["L = DDG`cotanLaplacian[mesh];   (* V x V SparseArray *)
Max[Abs[L - Transpose[L]]]            (* 0 : symmetric *)
Max[Abs[L . ConstantArray[1., Length[L]]]]   (* ~1e-15 : constants in kernel *)"],

(* ---------------------------------------------------------------- *)
hd1["3.  Spectral geometry: hearing the shape of a mesh"],
para[{"The eigenpairs of the Laplacian are a mesh's natural harmonics. Solving \
the generalized eigenproblem"}],
dm[HoldForm[-L . \[Phi] == \[Lambda] M . \[Phi]]],
para[{"gives eigenvalues (a shape ", ital["fingerprint"], ") and eigenfunctions \
(smooth standing waves). On the unit sphere the eigenvalues must be ", ital["l"],
   "(", ital["l"], "+1) = 0, 2, 6, 12, 20, \[Ellipsis] with multiplicity 2",
   ital["l"], "+1 \[LongDash] and the discrete operator delivers exactly that \
(2.000\[Times]3, 5.97\[Times]5, 11.8\[Times]7), while the eigenfunctions are the \
spherical harmonics:"}],
imgCell["eigenfunctions.png", 520],
captionCell["Figure 3. The first nine non-constant Laplace\[Dash]Beltrami \
eigenfunctions on a geodesic sphere \[LongDash] the discrete spherical harmonics \
", StyleBox["Y", FontSlant -> Italic], " for ", ital["l"], " = 1, 2, 3."],
para[{"Truncating the eigenbasis low-pass-filters geometry, exactly as a Fourier \
series does for signals: reconstructing a bumpy mesh from its lowest ", ital["K"],
   " manifold-harmonic modes recovers progressively finer detail."}],
imgCell["spectral_smoothing.png", 620],
captionCell["Figure 4. Spectral mesh smoothing: a bumpy sphere reconstructed from \
its lowest ", ital["K"], " Laplacian eigenmodes \[LongDash] the geometry analogue \
of Fourier compression."],

(* ---------------------------------------------------------------- *)
hd1["4.  Mean-curvature flow: smoothing as diffusion"],
para[{"Letting the Laplacian pull on the vertices themselves, ",
   StyleBox["\[PartialD]", FontSlant -> Italic], ital["x"], "/",
   StyleBox["\[PartialD]", FontSlant -> Italic], ital["t"], " = ",
   StyleBox["\[CapitalDelta]", FontSlant -> Italic], ital["x"],
   ", is ", bold["mean-curvature flow"], ": the surface diffuses, smoothing itself \
by its own curvature. Explicit time-stepping is stable only for tiny steps; the \
semi-implicit scheme (Desbrun\[Dash]Meyer\[Dash]Schr\[ODoubleDot]der\[Dash]Barr \
1999) is unconditionally stable and is one linear solve per step:"}],
dm[HoldForm[(M - \[Tau] L) . Superscript[x, "n+1"] == M . Superscript[x, "n"]]],
para[{"As a quantitative check, a round sphere under this flow obeys ", ital["R"],
   "(", ital["t"], ")", StyleBox["\[ThinSpace]2", FontSize -> 9], " = ", ital["R"],
   Subscript["", "0"], StyleBox["\[ThinSpace]2", FontSize -> 9], " \[Minus] 4",
   ital["t"], "; we recover the slope \[Minus]3.98 (theory \[Minus]4). Applied to a \
bumpy sphere \[LongDash] and rescaled to constant volume so the eye sees ", ital[
   "smoothing"], " rather than the shrinking that bare flow also produces \
\[LongDash] the surface relaxes to round and its mean curvature equalises:"}],
imgCell["mcf_before_after.png", 620],
captionCell["Figure 5. Mean-curvature flow (volume-preserved) faires a bumpy \
sphere; colour is mean curvature, which equalises as the shape rounds."],
animCell["mcf_flow.gif"],
captionCell["Animation. The same flow over time."],

(* ---------------------------------------------------------------- *)
hd1["5.  Outlook: geodesics and discrete exterior calculus"],
para[{"The same cotangent Laplacian computes ", bold["geodesic distance"],
   " via the heat method (Crane, Weischedel & Wardetzky 2013): diffuse heat \
briefly, normalise its gradient, then solve one Poisson problem ", ital["L"],
   StyleBox["\[Phi]", FontSlant -> Italic], " = div ", ital["X"], ". And building \
the discrete exterior derivative and Hodge-star operators yields a ",
   bold["Hodge decomposition"], " of tangent vector fields whose harmonic part \
counts the surface's handles. Both are the next sections of this project."}],

(* ---------------------------------------------------------------- *)
hd1["6.  References"],
ref["Meyer, M., Desbrun, M., Schr\[ODoubleDot]der, P., Barr, A. (2003). ",
  ital["Discrete differential-geometry operators for triangulated 2-manifolds."],
  " Visualization and Mathematics III."],
ref["Desbrun, M., Meyer, M., Schr\[ODoubleDot]der, P., Barr, A. (1999). ",
  ital["Implicit fairing of irregular meshes using diffusion and curvature flow."],
  " SIGGRAPH '99."],
ref["Crane, K., Weischedel, C., Wardetzky, M. (2013). ", ital["Geodesics in heat."],
  " ACM Transactions on Graphics. ",
  link["cs.cmu.edu/~kmcrane/Projects/HeatMethod",
    "https://www.cs.cmu.edu/~kmcrane/Projects/HeatMethod/"]],
ref["Crane, K. ", ital["Discrete Differential Geometry: An Applied Introduction"],
  " (CMU 15-458/858 course notes). ",
  link["cs.cmu.edu/~kmcrane/Projects/DDG",
    "https://www.cs.cmu.edu/~kmcrane/Projects/DDG/"]],
ref["Botsch, M., Kobbelt, L., Pauly, M., Alliez, P., L\[EAcute]vy, B. (2010). ",
  ital["Polygon Mesh Processing."], " AK Peters."],

(* ---------------------------------------------------------------- *)
hd1["7.  Reproducibility"],
para[{"All code is pure Wolfram Language. The shared package ",
   mono["wolfram/ddg.wl"], " defines the mesh accessors and operators; each \
section's figures are produced by a standalone script. Every operator is covered \
by a sanity check in ", mono["tests/"], " (Gauss\[Dash]Bonnet, mean-curvature \
convergence, the ", ital["l"], "(", ital["l"], "+1) spectrum, the flow rate)."}],
codeIn["wolframscript -file wolfram/curvature.wls      # \[Section]1 Gaussian curvature
wolframscript -file wolfram/meancurvature.wls  # \[Section]1-2 mean curvature
wolframscript -file wolfram/spectral.wls       # \[Section]3 spectrum + harmonics
wolframscript -file wolfram/meanflow.wls       # \[Section]4 mean-curvature flow
wolframscript -file community/build_notebook.wls   # this notebook"],
para[{Style["Built " <> DateString[{"MonthName", " ", "DayShort", ", ", "Year"}] <>
    " with Wolfram Language. Repository assembled with Claude Code.",
   FontSlant -> Italic, FontColor -> GrayLevel[0.45]]}]

}];

Print["assembled ", Length[cells], " cells"];

TimeConstrained[
  UsingFrontEnd[
    nbObj = CreateDocument[cells, Visible -> False, WindowSize -> {1000, 800},
      StyleDefinitions -> "Default.nb"];
    NotebookSave[nbObj, outNb];
    Print["wrote ", outNb, " (", FileByteCount[outNb], " B)"];
    Quiet@Check[
      Export[outPdf, nbObj];
      Print["wrote ", outPdf, " (", FileByteCount[outPdf], " B)"],
      Print["warn: PDF export failed"]];
    NotebookClose[nbObj];
  ],
  600,
  Print["warn: notebook save timed out"]];
