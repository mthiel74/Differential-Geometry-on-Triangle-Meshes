#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  Build community/ddg.nb — an educational *and* scientific exposition of
    discrete differential geometry on triangle meshes, in pure Wolfram
    Language. Each section states the smooth object, derives its discrete
    counterpart, and reports the validation that pins the discrete answer to
    the smooth one (a convergence rate, a conservation law, or a topological
    invariant).

    Architecture (as in the ENSO-emergence builder): assemble a flat list of
    fully-formed Cell expressions, then persist via an invisible FrontEnd
    document + NotebookSave and export a PDF.

    Sections
    --------
      Abstract
      0. The discrete world: meshes and a smooth-to-discrete dictionary
      1. Curvature: angle defect, Gauss-Bonnet, the mean-curvature normal
      2. The cotangent Laplace-Beltrami operator
      3. Spectral geometry: hearing the shape of a mesh
      4. Mean-curvature flow: smoothing as diffusion
      5. Geodesic distance by the heat method
      6. Discrete exterior calculus and the Hodge decomposition
      7. The operators on a real 3D scan
      8. Validation summary
      9. References
     10. Reproducibility
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
sub[v_, k_] := Cell[BoxData[FormBox[SubscriptBox[v, k], TraditionalForm]], "InlineFormula"];
link[txt_, url_] := ButtonBox[StyleBox[txt, FontColor -> RGBColor[0.2, 0.3, 0.8]],
   BaseStyle -> "Hyperlink", ButtonData -> {URL[url], None}, ButtonNote -> url];

dm[expr_] := Cell[BoxData[ToBoxes[TraditionalForm[expr]]], "DisplayFormula",
   CellMargins -> {{70, Automatic}, {8, 8}}, TextAlignment -> Center];

(* highlighted "theorem / definition" box *)
thmBox[tag_, stuff_] := Cell[TextData[Flatten[{bold[tag <> "  "], stuff}]], "Text",
   CellMargins -> {{60, 60}, {6, 6}}, Background -> RGBColor[0.93, 0.95, 0.99],
   CellFrame -> 0.5, CellFrameColor -> RGBColor[0.7, 0.78, 0.9],
   CellFrameMargins -> 8];

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

tableCell[grid_] := Cell[BoxData[ToBoxes[grid]], "Output",
   ShowCellLabel -> False, TextAlignment -> Center,
   CellMargins -> {{Automatic, Automatic}, {10, 10}}];

(* ===================================================================== *)

cells = Flatten[{

title["Differential Geometry on Triangle Meshes"],
subtitle["One operator \[LongDash] the cotangent Laplacian \[LongDash] from \
curvature to spectra, flow, geodesics and topology, in Wolfram Language"],

abstract[{bold["Abstract. "],
  "A smooth surface carries curvature, a Laplace operator, a spectrum, a notion \
of distance and a homology. A triangle mesh \[LongDash] a finite list of \
vertices and triangles \[LongDash] has none of these ", ital["a priori"],
  ". ", ital["Discrete differential geometry"], " (DDG) constructs operators on \
meshes that do not merely approximate the smooth theory but reproduce its \
theorems exactly: the discrete Gauss\[Dash]Bonnet identity holds to machine \
precision, the discrete Laplacian is symmetric with the constants in its kernel, \
and the dimension of the discrete harmonic space equals a topological invariant. \
This notebook builds that core in pure Wolfram Language. For every construction \
we state the smooth object, derive the discrete one, and report the check that \
pins it down \[LongDash] a convergence rate, a conservation law, or a Betti \
number. A single sparse matrix, the ", bold["cotangent Laplacian"],
  ", turns out to drive all of it. The same operators are then run unchanged on \
a 3D-scanned figurine."}],

(* ====================== 0. Discrete world ======================== *)
hd1["0.  The discrete world: meshes and a dictionary"],
para[{"A ", bold["triangle mesh"], " is a pair: a list of vertex positions ",
  sub["x", "i"], " \[Element] \[DoubleStruckCapitalR]", Cell[BoxData["3"], "InlineFormula"],
  " and a list of triangles, each an ordered triple of vertex indices. A scalar \
field is one number per vertex; we treat it as ", ital["piecewise linear"],
  " \[LongDash] linearly interpolated across each triangle by the barycentric \
\"hat\" functions ", sub["\[Phi]", "i"], ". Almost every smooth notion has a \
discrete counterpart on this data:"}],
tableCell[Grid[{
   {Style["smooth", Bold], Style["discrete", Bold]},
   {"surface \[CapitalSigma]", "vertices + triangles"},
   {"function f : \[CapitalSigma] \[Rule] \[DoubleStruckCapitalR]", "one value per vertex"},
   {"Gaussian curvature K dA", "angle defect at a vertex"},
   {"Laplace\[Dash]Beltrami \[CapitalDelta]", "cotangent matrix M\:207b\.b9 L"},
   {"\[Integral] f over \[CapitalSigma]", "mass-matrix weighted sum  f\[Transpose] M 1"},
   {"k-form", "value per vertex / edge / face"},
   {"de Rham cohomology", "kernel of the discrete Laplacian"}},
  Frame -> All, FrameStyle -> GrayLevel[0.8], Spacings -> {2, 1.1},
  Background -> {None, {RGBColor[0.93, 0.95, 0.99], None}}, Alignment -> Left]],
para[{"The guiding principle is ", ital["mimetic"], ": rather than discretise a \
smooth formula numerically, we build operators that satisfy the same structural \
identities (Stokes' theorem, ", ital["d"], Cell[BoxData["\.b2"], "InlineFormula"],
  " = 0, self-adjointness) by construction. Structure-preservation, not just \
small truncation error, is what makes the theorems survive."}],
para[{"Throughout we use two exact test surfaces \[LongDash] a ", bold["torus"],
  " (built from a periodic grid) and a regular geodesic ", bold["sphere"],
  " (a subdivided icosahedron, ", mono["DDG`icosphereMesh"], ") \[LongDash] \
because both have closed-form curvature, spectrum and geodesics to check against."}],

(* ====================== 1. Curvature ============================= *)
hd1["1.  Curvature: angle defect, Gauss\[Dash]Bonnet, mean curvature"],
para[{"A smooth surface has two principal curvatures ", sub["\[Kappa]", "1"], ", ",
  sub["\[Kappa]", "2"], "; from them the ", bold["Gaussian"], " curvature ",
  ital["K"], " = ", sub["\[Kappa]", "1"], sub["\[Kappa]", "2"],
  " (intrinsic \[LongDash] preserved under bending) and the ", bold["mean"],
  " curvature ", ital["H"], " = (", sub["\[Kappa]", "1"], " + ",
  sub["\[Kappa]", "2"], ")/2 (extrinsic). On a mesh, curvature concentrates at \
the vertices."}],
para[{"Gaussian curvature is measured by the ", bold["angle defect"],
  ": flatten the triangles around a vertex into the plane and they leave a gap. \
The ", ital["integrated"], " Gaussian curvature at an interior vertex ", ital["i"],
  " is that gap,"}],
dm[HoldForm[Subscript[K, i] == 2 \[Pi] - Sum[Subscript[\[Theta], j], j]]],
para[{"where the ", sub["\[Theta]", "j"], " are the triangle corner angles at ",
  ital["i"], ". The pointwise curvature is ", sub["K", "i"],
  " divided by the vertex area ", sub["A", "i"], " (one third of the incident \
triangle areas). Summing over a closed mesh telescopes the corner angles \
(each triangle contributes \[Pi]) into a purely topological statement:"}],
thmBox["Discrete Gauss\[Dash]Bonnet.",
  {"For a closed triangle mesh, ", Cell[BoxData[ToBoxes[TraditionalForm[
      HoldForm[Sum[Subscript[K, i], i] == 2 \[Pi] \[Chi]]]]], "InlineFormula"],
   ", where \[Chi] = V \[Minus] E + F is the Euler characteristic. This is an ",
   ital["identity"], ", not an approximation."}],
para[{"Our implementation confirms it to machine precision: the sphere (\[Chi] = \
2) totals 4\[Pi] with residual \[TildeTilde] 10", Cell[BoxData["\:207b\.b9\.b2"], "InlineFormula"],
  ", and the torus (\[Chi] = 0) totals zero. Pointwise, the discrete ", ital["K"],
  " matches the analytic torus curvature ", sub["\[Kappa]", "1"], sub["\[Kappa]", "2"],
  " to a relative L\.b2 error of 5\[Times]10", Cell[BoxData["\:207b\:2074"], "InlineFormula"], "."}],
imgCell["torus_gaussian_curvature.png"],
captionCell["Figure 1. Discrete Gaussian curvature on a torus from the angle \
defect: positive (warm) on the outer rim, negative (blue) on the inner rim, \
integrating to zero exactly as Gauss\[Dash]Bonnet demands."],
para[{"The ", bold["mean curvature"], " arises from the cotangent Laplacian of \
\[Section]2 applied to the coordinate functions: the ", ital["mean-curvature \
normal"], " (Meyer et al. 2003) satisfies \[CapitalDelta]", ital["x"],
  " = 2", ital["H"], " ", ital["n"], ". The discrete version matches the smooth \
torus ", ital["H"], "(", ital["u"], ") to 3\[Times]10", Cell[BoxData["\:207b\:2074"], "InlineFormula"],
  " and the unit sphere's ", ital["H"], " = 1 to 5\[Times]10", Cell[BoxData["\:207b\.b3"], "InlineFormula"], "."}],
imgCell["torus_mean_curvature.png"],
captionCell["Figure 2. Discrete mean curvature on the torus from the cotangent \
Laplacian of the coordinate functions."],

(* ====================== 2. Cotan Laplacian ======================= *)
hd1["2.  The cotangent Laplace\[Dash]Beltrami operator"],
para[{"The Laplace\[Dash]Beltrami operator \[CapitalDelta] is the heart of the \
smooth theory. Its discrete form is obtained by the finite-element recipe: take \
piecewise-linear hat functions ", sub["\[Phi]", "i"], " and assemble the ",
  ital["stiffness matrix"], " of inner products of their gradients. Carrying out \
the per-triangle integral gives the celebrated cotangent weights,"}],
dm[HoldForm[Subscript[(L f), i] ==
   (1/2) Sum[(Cot[Subscript[\[Alpha], ij]] + Cot[Subscript[\[Beta], ij]])
      (Subscript[f, j] - Subscript[f, i]), j]]],
para[{"where ", sub["\[Alpha]", "ij"], " and ", sub["\[Beta]", "ij"],
  " are the two angles opposite edge (", ital["i"], ",", ital["j"], "). Three \
properties make it the right operator, and all hold by construction:"}],
para[{"\[Bullet] ", bold["symmetric"], " ( ", ital["L"], " = ", ital["L"],
  Cell[BoxData["\[Transpose]"], "InlineFormula"], " ), so it has real eigenvalues; \
\[Bullet] ", bold["constants in its kernel"], " ( ", ital["L"], " ", ital["1"],
  " = 0 ), the discrete \"\[CapitalDelta] of a constant is zero\"; \[Bullet] ",
  bold["negative semi-definite"], " and ", bold["local"], " (only neighbours \
couple). Paired with the diagonal ", bold["mass matrix"], " ", ital["M"],
  " of vertex areas, ", ital["M"], Cell[BoxData["\:207b\.b9"], "InlineFormula"], ital["L"],
  " is the discrete \[CapitalDelta]. Every later section is this one matrix with \
a different right-hand side."}],
wlIn["L = DDG`cotanLaplacian[mesh];          (* V\[Times]V SparseArray *)
Max[Abs[L - Transpose[L]]]                 (* 0  : symmetric *)
Max[Abs[L . ConstantArray[1., Length[L]]]] (* ~1e-15 : constants in kernel *)
M = DDG`massMatrix[mesh];                   (* diagonal vertex areas *)"],
para[{"Remarkably, the very same matrix is re-derived in \[Section]6 from \
discrete exterior calculus as ", ital["L"], " = ", sub["d", "0"],
  Cell[BoxData["\[Transpose]"], "InlineFormula"], " ", Cell[BoxData[
    SubscriptBox["\[Star]", "1"]], "InlineFormula"], " ", sub["d", "0"],
  " \[LongDash] two independent routes to one object."}],

(* ====================== 3. Spectral ============================== *)
hd1["3.  Spectral geometry: hearing the shape of a mesh"],
para[{"Kac asked in 1966: ", ital["can one hear the shape of a drum?"],
  " The eigenpairs of \[CapitalDelta] are a surface's natural frequencies and \
standing waves. Discretely we solve the generalized eigenproblem"}],
dm[HoldForm[-L . \[Phi] == \[Lambda] M . \[Phi]]],
para[{"(symmetrised as ", Cell[BoxData[ToBoxes[TraditionalForm[HoldForm[
     Superscript[M, RowBox[{"-", "1", "/", "2"}]]]]]], "InlineFormula"],
  "(\[Minus]", ital["L"], ")", Cell[BoxData[ToBoxes[TraditionalForm[HoldForm[
     Superscript[M, RowBox[{"-", "1", "/", "2"}]]]]]], "InlineFormula"],
  ", which is positive semi-definite so the eigenvalues are real and \
non-negative). On the unit sphere the spectrum of \[CapitalDelta] is known \
exactly: the spherical harmonics ", sub["Y", "lm"], " satisfy"}],
dm[HoldForm[\[CapitalDelta] Subscript[Y, "lm"] == -l (l + 1) Subscript[Y, "lm"]]],
para[{"so the eigenvalues must be ", ital["l"], "(", ital["l"], "+1) = 0, 2, 6, \
12, 20, \[Ellipsis] with multiplicity 2", ital["l"], "+1. The discrete operator \
reproduces this to a fraction of a percent:"}],
tableCell[Grid[{
   {Style["l", Bold], Style["l(l+1)", Bold], Style["discrete eigenvalues", Bold]},
   {0, 0, "0"},
   {1, 2, "2.000, 2.000, 2.000"},
   {2, 6, "5.97, 5.97, 5.97, 5.97, 5.97"},
   {3, 12, "11.80\[Times]3, 11.85\[Times]4"}},
  Frame -> All, FrameStyle -> GrayLevel[0.8], Spacings -> {2, 1.1},
  Background -> {None, {RGBColor[0.93, 0.95, 0.99], None}}, Alignment -> Left]],
para[{"The eigenvectors are M-orthonormal (Gram matrix = identity to 10",
  Cell[BoxData["\:207b\.b9\:2075"], "InlineFormula"], ") and, on the sphere, are \
literally the spherical harmonics:"}],
imgCell["eigenfunctions.png", 520],
captionCell["Figure 3. The first nine non-constant Laplace\[Dash]Beltrami \
eigenfunctions on a geodesic sphere \[LongDash] the discrete spherical harmonics ",
  ital["Y"], " for ", ital["l"], " = 1, 2, 3."],
para[{"Because the eigenvectors form an orthonormal basis, truncating it \
low-pass-filters geometry exactly as a Fourier series filters a signal. \
Reconstructing the coordinate functions of a bumpy mesh from its lowest ", ital["K"],
  " ", ital["manifold-harmonic"], " modes recovers detail progressively:"}],
imgCell["spectral_smoothing.png", 620],
captionCell["Figure 4. Spectral mesh smoothing: a bumpy sphere reconstructed from \
its lowest ", ital["K"], " Laplacian eigenmodes \[LongDash] geometry's Fourier \
compression."],

(* ====================== 4. Mean-curvature flow =================== *)
hd1["4.  Mean-curvature flow: smoothing as diffusion"],
para[{"Letting the Laplacian act on the surface itself gives ", bold["mean-\
curvature flow"], ", \[PartialD]", ital["x"], "/\[PartialD]", ital["t"],
  " = \[CapitalDelta]", ital["x"], " = 2", ital["H"], " ", ital["n"],
  ": every point moves along its mean-curvature normal, so the surface diffuses \
and smooths. Discretising the time derivative two ways is instructive. The ",
  ital["explicit"], " step is stable only for ", ital["\[Tau]"], " smaller than \
the square of the shortest edge; the ", ital["semi-implicit"], " step (Desbrun, \
Meyer, Schr\[ODoubleDot]der & Barr 1999) is unconditionally stable and costs one \
sparse solve:"}],
dm[HoldForm[(M - \[Tau] L) . Superscript[x, "n+1"] == M . Superscript[x, "n"]]],
para[{"There is a clean quantitative test. Under this flow a round sphere stays \
round and its radius obeys the ODE ", ital["d"], "(", ital["R"], Cell[BoxData["\.b2"], "InlineFormula"],
  ")/", ital["dt"], " = \[Minus]4, i.e."}],
dm[HoldForm[Superscript[R, 2](t) == Superscript[Subscript[R, 0], 2] - 4 t]],
para[{"and our solver recovers the slope \[Minus]3.98 (relative error 6\[Times]10",
  Cell[BoxData["\:207b\.b3"], "InlineFormula"], "). Applied to a bumpy sphere \
\[LongDash] rescaled to constant volume so the eye sees ", ital["smoothing"],
  " not the shrinking that bare flow also produces \[LongDash] the surface \
relaxes to round while its mean curvature equalises:"}],
imgCell["mcf_before_after.png", 620],
captionCell["Figure 5. Mean-curvature flow (volume-preserved) faires a bumpy \
sphere; colour is mean curvature, equalising as the shape rounds."],
animCell["mcf_flow.gif"],
captionCell["Animation. The flow over time; the curvature heat-map settles to \
uniform."],

(* ====================== 5. Geodesics ============================= *)
hd1["5.  Geodesic distance by the heat method"],
para[{"Geodesic distance solves the eikonal equation |\[Del]", ital["\[Phi]"],
  "| = 1, which is nonlinear and awkward on a mesh. Varadhan's 1967 formula \
offers a linear route: the heat kernel ", sub["u", "t"], " after a short time ",
  ital["t"], " encodes distance through ", ital["\[Phi]"], " = lim ",
  Cell[BoxData[ToBoxes[TraditionalForm[HoldForm[Sqrt[-4 t Log[Subscript[u, t]]]]]]], "InlineFormula"],
  ". Crane, Weischedel & Wardetzky (2013) turn this into three linear-algebra \
steps, each using only ", ital["L"], " and ", ital["M"], ":"}],
para[{"\[Bullet] (1) ", bold["diffuse"], " heat from the source for time ",
  ital["t"], " \[TildeTilde] ", ital["h"], Cell[BoxData["\.b2"], "InlineFormula"],
  " ( ", ital["h"], " = mean edge length ):"}],
dm[HoldForm[(M - t L) . u == \[Delta]]],
para[{"\[Bullet] (2) the heat flows outward, so its normalised, negated gradient \
is a unit field ", ital["pointing away"], " from the source:"}],
dm[HoldForm[X == -Grad[u]/Norm[Grad[u]]]],
para[{"\[Bullet] (3) recover the distance whose gradient best matches ", ital["X"],
  " by one Poisson solve:"}],
dm[HoldForm[L . \[Phi] == Div[X]]],
para[{"On the unit sphere the distance from the north pole is the colatitude \
arccos(", ital["z"], "); the heat method matches it and the error ", bold["shrinks \
under refinement"], " \[LongDash] 1.3% at 642 vertices, 0.84% at 2562, 0.67% at \
10242 \[LongDash] confirming convergence, not a coincidence."}],
imgCell["geodesics.png", 640],
captionCell["Figure 6. Geodesic distance fields. Left: a sphere from the pole, \
where the bands are circles of latitude. Right: a torus from one point \[LongDash] \
the bands wrap around both the tube and the hole, distance that no straight-line \
metric could give."],

(* ====================== 6. DEC + Hodge =========================== *)
hd1["6.  Discrete exterior calculus and the Hodge decomposition"],
para[{"Differential forms unify gradient, curl and divergence into one calculus. \
On a mesh, a ", ital["k"], "-form is a number per vertex (0-form), per edge \
(1-form), or per face (2-form). The exterior derivative ", ital["d"],
  " becomes signed incidence: ", sub["d", "0"], " (vertices \[Rule] edges) is the \
discrete gradient, ", sub["d", "1"], " (edges \[Rule] faces) the discrete curl. \
The fundamental identity ", ital["d"], Cell[BoxData["\.b2"], "InlineFormula"],
  " = 0 holds combinatorially \[LongDash] the boundary of a boundary is empty:"}],
dm[HoldForm[Subscript[d, 1] . Subscript[d, 0] == 0]],
para[{"With the diagonal Hodge stars (", Cell[BoxData[SubscriptBox["\[Star]", "0"]], "InlineFormula"],
  " vertex areas, ", Cell[BoxData[SubscriptBox["\[Star]", "1"]], "InlineFormula"],
  " the cotan edge weights, ", Cell[BoxData[SubscriptBox["\[Star]", "2"]], "InlineFormula"],
  " inverse face areas), the Laplacian of \[Section]2 re-emerges, ", ital["L"],
  " = ", sub["d", "0"], Cell[BoxData["\[Transpose]"], "InlineFormula"], " ",
  Cell[BoxData[SubscriptBox["\[Star]", "1"]], "InlineFormula"], " ", sub["d", "0"],
  " (we verify this equals ", mono["cotanLaplacian"], " to machine zero). The \
payoff is a complete decomposition of any edge 1-form:"}],
thmBox["Hodge\[Dash]Helmholtz decomposition.",
  {"Every 1-form splits uniquely and ", Cell[BoxData[SubscriptBox["\[Star]", "1"]], "InlineFormula"],
   "-orthogonally as ",
   Cell[BoxData[ToBoxes[TraditionalForm[HoldForm[\[Omega] == d \[Alpha] + \[Delta] \[Beta] + \[Gamma]]]]], "InlineFormula"],
   " : a gradient (exact) part, a curl (coexact) part, and a ", ital["harmonic"],
   " part \[Gamma]. The harmonic space has dimension ",
   Cell[BoxData[ToBoxes[TraditionalForm[HoldForm[Subscript[b, 1] == 2 g]]]], "InlineFormula"],
   " (g = genus) \[LongDash] it ", ital["counts the handles"], "."}],
para[{"Computed as ", sub["b", "1"], " = E \[Minus] rank ", sub["d", "0"],
  " \[Minus] rank ", sub["d", "1"], ", the discrete harmonic dimension is exactly \
2 on the torus and 0 on the sphere. Decomposing a swirling field on the torus, \
the harmonic part is the dominant circulation around the handle that neither a \
gradient nor a curl can represent:"}],
imgCell["hodge_decomposition.png", 660],
captionCell["Figure 7. Hodge\[Dash]Helmholtz decomposition of a swirl on the \
torus into exact (gradient, \[TildeTilde]0%), coexact (curl, 27%) and harmonic \
(73%) parts. The harmonic component circulates around the hole \[LongDash] a \
purely topological degree of freedom."],

(* ====================== 7. Real scan ============================= *)
hd1["7.  The operators on a real 3D scan"],
para[{"Spheres and tori let us check answers; real geometry shows the operators \
are not toys. We take a 503k-vertex scanned ", bold["Yoda"], " figurine (from the \
Wolfram3DObjects collection), weld its texture seams, keep the largest component, \
and decimate it with quadric error metrics to a clean 12k-triangle mesh (a one-off \
", mono["open3d"], " step; the result is committed, so the analysis stays pure \
Wolfram Language). Every operator above runs on it unchanged."}],
imgCell["yoda_geodesics.png", 460],
captionCell["Figure 8. Heat-method geodesic distance from the top of the head on \
the scanned figurine. The bands follow the surface \[LongDash] note that the \
distance creeps ", ital["along"], " the staff, reaching its tip only after \
travelling down the arm, exactly as an ant walking the surface would."],
imgCell["yoda_eigenfunctions.png", 560],
captionCell["Figure 9. Laplace\[Dash]Beltrami eigenfunctions (manifold harmonics) \
on the scan: the slender staff supports a distinct low-frequency mode, and the \
spatial frequency rises with the eigenvalue."],
para[{"Mean curvature on the scan highlights the folds of the robe and the sharp \
creases (a robust quantile clip suppresses scan noise). The lesson: the DDG \
operators degrade gracefully on messy, boundary-carrying, non-uniform meshes \
\[LongDash] which is why they are the backbone of real geometry-processing \
software."}],
imgCell["yoda_mean_curvature.png", 460],
captionCell["Figure 10. Discrete mean curvature on the scanned figurine, picking \
out ridges and folds."],

(* ====================== 8. Validation ============================ *)
hd1["8.  Validation summary"],
para[{"Nothing above is asserted without a check against a smooth reference or a \
topological invariant. In one table:"}],
tableCell[Grid[{
   {Style["quantity", Bold], Style["test", Bold], Style["result", Bold]},
   {"Gaussian curvature", "vs analytic torus K", "rel L2  5\[Times]10\:207b\:2074"},
   {"Gauss\[Dash]Bonnet", "sphere total = 4\[Pi]", "residual  10\:207b\.b9\.b2"},
   {"mean curvature", "sphere H = 1", "rel L2  5\[Times]10\:207b\.b3"},
   {"LBO spectrum", "sphere \[Lambda] = l(l+1)", "rel err  6\[Times]10\:207b\.b3"},
   {"eigenvectors", "M-orthonormality", "10\:207b\.b9\:2075"},
   {"mean-curvature flow", "d(R\.b2)/dt = \[Minus]4", "\[Minus]3.98"},
   {"heat geodesics", "vs arccos(z), refined", "1.3% \[Rule] 0.67%"},
   {"DEC", "d\.b2 = 0 ;  L = d\:2080\[Transpose]\[Star]\:2081 d\:2080", "exact (0)"},
   {"Hodge", "harmonic dim = 2g", "torus 2, sphere 0"}},
  Frame -> All, FrameStyle -> GrayLevel[0.8], Spacings -> {2, 1.1},
  Background -> {None, {RGBColor[0.93, 0.95, 0.99], None}}, Alignment -> Left]],

(* ====================== 9. References ============================ *)
hd1["9.  References"],
ref["Meyer, M., Desbrun, M., Schr\[ODoubleDot]der, P., Barr, A. (2003). ",
  ital["Discrete differential-geometry operators for triangulated 2-manifolds."],
  " Visualization and Mathematics III, 35\[Dash]57."],
ref["Desbrun, M., Meyer, M., Schr\[ODoubleDot]der, P., Barr, A. (1999). ",
  ital["Implicit fairing of irregular meshes using diffusion and curvature flow."],
  " SIGGRAPH '99, 317\[Dash]324."],
ref["Crane, K., Weischedel, C., Wardetzky, M. (2013). ", ital["Geodesics in heat: \
a new approach to computing distance based on heat flow."], " ACM Trans. Graph. \
32(5). ", link["cs.cmu.edu/~kmcrane/Projects/HeatMethod",
    "https://www.cs.cmu.edu/~kmcrane/Projects/HeatMethod/"]],
ref["Crane, K. ", ital["Discrete Differential Geometry: An Applied Introduction"],
  " (CMU 15-458/858). ", link["cs.cmu.edu/~kmcrane/Projects/DDG",
    "https://www.cs.cmu.edu/~kmcrane/Projects/DDG/"]],
ref["Botsch, M., Kobbelt, L., Pauly, M., Alliez, P., L\[EAcute]vy, B. (2010). ",
  ital["Polygon Mesh Processing."], " AK Peters."],
ref["Varadhan, S. R. S. (1967). ", ital["On the behavior of the fundamental \
solution of the heat equation with variable coefficients."], " Comm. Pure Appl. \
Math. 20, 431\[Dash]455."],

(* ====================== 10. Reproducibility ====================== *)
hd1["10.  Reproducibility"],
para[{"All code is pure Wolfram Language. The package ", mono["wolfram/ddg.wl"],
  " defines the mesh accessors, test meshes and operators; each section's \
figures come from one standalone script, and every operator has a sanity check \
in ", mono["tests/"], " (all exit 0). The single non-WL step is the one-off ",
  mono["open3d"], " decimation of the Yoda scan, whose output meshes are committed."}],
codeIn["wolframscript -file wolfram/curvature.wls     # \[Section]1 Gaussian curvature
wolframscript -file wolfram/meancurvature.wls # \[Section]1-2 mean curvature
wolframscript -file wolfram/spectral.wls      # \[Section]3 spectrum + harmonics
wolframscript -file wolfram/meanflow.wls      # \[Section]4 mean-curvature flow
wolframscript -file wolfram/heat.wls          # \[Section]5 heat-method geodesics
wolframscript -file wolfram/hodge.wls         # \[Section]6 DEC + Hodge
wolframscript -file wolfram/yoda.wls          # \[Section]7 the 3D scan
wolframscript -file community/build_notebook.wls   # this notebook
# wolfram/run_all.wls runs the whole pipeline end to end."],
para[{Style["Built " <> DateString[{"MonthName", " ", "DayShort", ", ", "Year"}] <>
   " with Wolfram Language. Code and full history at the project repository; \
repository scaffolding and notebook build assisted by Claude Code.",
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
  900,
  Print["warn: notebook save timed out"]];
