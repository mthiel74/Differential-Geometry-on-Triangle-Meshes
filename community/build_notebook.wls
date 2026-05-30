#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  Build community/ddg.nb — an educational *and* scientific exposition of
    discrete differential geometry on triangle meshes, in pure Wolfram
    Language. Each section states the smooth object, derives its discrete
    counterpart, reports the validation (a convergence rate, a conservation
    law, or a topological invariant), and embeds the result as a LIVE,
    rotatable Graphics3D the reader can drag.

    3D figures are imported from docs/models/*.wxf (built by wolfram/models.wls)
    and embedded as interactive Graphics3DBox cells. Multi-panel comparison
    galleries stay as committed PNGs from docs/images/.
*)

repoRoot = ParentDirectory@DirectoryName[$InputFileName];
imgDir   = FileNameJoin[{repoRoot, "docs", "images"}];
modelDir = FileNameJoin[{repoRoot, "docs", "models"}];
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
   BaseStyle -> "Hyperlink", ButtonData -> {URL[url], None}, ButtonNote -> url];

dm[expr_] := Cell[BoxData[ToBoxes[TraditionalForm[expr]]], "DisplayFormula",
   CellMargins -> {{70, Automatic}, {8, 8}}, TextAlignment -> Center];

thmBox[tag_, stuff_] := Cell[TextData[Flatten[{bold[tag <> "  "], stuff}]], "Text",
   CellMargins -> {{60, 60}, {6, 6}}, Background -> RGBColor[0.93, 0.95, 0.99],
   CellFrame -> 0.5, CellFrameColor -> RGBColor[0.7, 0.78, 0.9], CellFrameMargins -> 8];

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

(* LIVE rotatable 3D: import the Graphics3D and embed its boxes directly *)
liveCell[name_, width_: 430] := With[{p = FileNameJoin[{modelDir, name <> ".wxf"}]},
   If[FileExistsQ[p],
     Cell[BoxData[ToBoxes[Show[Import[p], ImageSize -> width]]], "Output",
       ShowCellLabel -> False, TextAlignment -> Center,
       CellMargins -> {{Automatic, Automatic}, {8, 4}}],
     Cell[TextData[{bold["[ missing model: " <> name <> " ]"]}], "Text",
       FontColor -> RGBColor[0.7, 0.1, 0.1], TextAlignment -> Center]]];

liveRow[names_, width_: 300] := With[
   {gs = (FileNameJoin[{modelDir, # <> ".wxf"}] &) /@ names},
   If[AllTrue[gs, FileExistsQ],
     Cell[BoxData[ToBoxes[Row[(Show[Import[#], ImageSize -> width] &) /@ gs,
        Spacer[8]]]], "Output", ShowCellLabel -> False, TextAlignment -> Center,
       CellMargins -> {{Automatic, Automatic}, {8, 4}}],
     Cell[TextData[{bold["[ missing models ]"]}], "Text",
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
tableCell[grid_] := Cell[BoxData[ToBoxes[grid]], "Output", ShowCellLabel -> False,
   TextAlignment -> Center, CellMargins -> {{Automatic, Automatic}, {10, 10}}];

rotNote := captionCell[bold["This is a live 3D object \[LongDash] click and drag to rotate it."]];
e[s_] := Cell[BoxData[s], "InlineFormula"];   (* inline box fragment *)

(* ===================================================================== *)

cells = Flatten[{

title["Differential Geometry on Triangle Meshes"],
subtitle["One operator \[LongDash] the cotangent Laplacian \[LongDash] from \
curvature to spectra, flow, geodesics and topology, in Wolfram Language"],

abstract[{bold["Abstract. "],
  "A smooth surface carries curvature, a Laplace operator, a spectrum, a notion \
of distance, and a homology. A triangle mesh \[LongDash] a finite list of \
vertices and triangles \[LongDash] has none of these ", ital["a priori"],
  ". ", ital["Discrete differential geometry"], " (DDG) constructs operators on \
meshes that do not merely approximate the smooth theory but reproduce its \
theorems exactly: discrete Gauss\[Dash]Bonnet holds to machine precision, the \
discrete Laplacian is symmetric with the constants in its kernel, and the \
dimension of the discrete harmonic space equals a topological invariant. This \
notebook builds that core in pure Wolfram Language. For every construction we \
state the smooth object, derive the discrete one, and report the check that pins \
it down \[LongDash] including the measured ", ital["order of accuracy"],
  ". A single sparse matrix, the ", bold["cotangent Laplacian"],
  ", drives all of it. The 3D figures are ", bold["live: drag to rotate them"],
  ". The same operators are then run unchanged on a 3D-scanned figurine, and \
finally pushed beyond meshes to ", bold["graphs"], " \[LongDash] Ollivier-Ricci \
curvature and emergent dimension \[LongDash] bridging to the discrete geometry of \
the ", bold["Wolfram Physics Project"], "."}],

(* ====================== 0. Discrete world ======================== *)
hd1["0.  The discrete world: meshes and a dictionary"],
para[{"A ", bold["triangle mesh"], " is a pair: vertex positions ",
  e[SubscriptBox["x", "i"]], " \[Element] \[DoubleStruckCapitalR]", e["3"],
  " and triangles, each an ordered triple of vertex indices. A scalar field is \
one number per vertex, interpolated across each triangle by the barycentric \
\"hat\" functions ", e[SubscriptBox["\[Phi]", "i"]], " (so fields are ",
  ital["piecewise linear"], "). Almost every smooth notion has a discrete \
counterpart on this data:"}],
tableCell[Grid[{
   {Style["smooth", Bold], Style["discrete", Bold]},
   {"surface \[CapitalSigma]", "vertices + triangles"},
   {"function f : \[CapitalSigma] \[Rule] \[DoubleStruckCapitalR]", "one value per vertex"},
   {"Gaussian curvature K dA", "angle defect at a vertex"},
   {"Laplace\[Dash]Beltrami \[CapitalDelta]", "cotangent matrix M\:207b\.b9 L"},
   {"\[Integral] f over \[CapitalSigma]", "mass-weighted sum  f\[Transpose] M 1"},
   {"k-form", "value per vertex / edge / face"},
   {"de Rham cohomology", "kernel of the Hodge Laplacian"}},
  Frame -> All, FrameStyle -> GrayLevel[0.8], Spacings -> {2, 1.1},
  Background -> {None, {RGBColor[0.93, 0.95, 0.99], None}}, Alignment -> Left]],
para[{"The guiding principle is ", ital["mimetic"], ": rather than discretise a \
smooth formula numerically, we build operators that satisfy the same structural \
identities (Stokes' theorem, ", ital["d"], e["\.b2"], " = 0, self-adjointness) \
by construction. Structure-preservation, not merely small truncation error, is \
what keeps the theorems exact. Two closed-form test surfaces \[LongDash] a ",
  bold["torus"], " and a geodesic ", bold["sphere"], " (a subdivided \
icosahedron) \[LongDash] let us check every answer."}],

(* ====================== 1. Curvature ============================= *)
hd1["1.  Curvature: the shape operator, angle defect, Gauss\[Dash]Bonnet"],
para[{"At a point of a smooth surface, the ", bold["shape operator"], " ",
  ital["S"], " = \[Minus]", ital["dn"], " (the derivative of the unit normal) is \
a self-adjoint map of the tangent plane. Its eigenvalues are the ", bold[
  "principal curvatures"], " ", e[SubscriptBox["\[Kappa]", "1"]], ", ",
  e[SubscriptBox["\[Kappa]", "2"]], ", and from them come the two scalar \
curvatures:"}],
dm[HoldForm[K == Det[S] == Subscript[\[Kappa], 1] Subscript[\[Kappa], 2]]],
dm[HoldForm[H == (1/2) Tr[S] == (Subscript[\[Kappa], 1] + Subscript[\[Kappa], 2])/2]],
para[{"The Gaussian curvature ", ital["K"], " is ", bold["intrinsic"],
  " \[LongDash] Gauss's ", ital["Theorema Egregium"], " says it depends only on \
lengths and angles within the surface, not on how it bends in space. That is \
exactly why a mesh can capture it from angles alone: flatten the triangles \
around a vertex and they leave an angular gap, the ", bold["angle defect"], ","}],
dm[HoldForm[Subscript[K, i] == 2 \[Pi] - Sum[Subscript[\[Theta], j], j]]],
para[{"This is the integrated Gaussian curvature at vertex ", ital["i"],
  "; dividing by the vertex area ", e[SubscriptBox["A", "i"]], " gives the \
pointwise value. Summing over a closed mesh, the corner angles telescope (each \
triangle's three angles sum to \[Pi]) into a purely topological identity:"}],
thmBox["Discrete Gauss\[Dash]Bonnet.",
  {"For a closed triangle mesh, ", e[ToBoxes[TraditionalForm[
      HoldForm[Sum[Subscript[K, i], i] == 2 \[Pi] \[Chi]]]]],
   ", with \[Chi] = V \[Minus] E + F the Euler characteristic. An exact ",
   ital["identity"], ", true at every resolution \[LongDash] not an approximation."}],
para[{"The mean curvature ", ital["H"], " is ", bold["extrinsic"],
  " and comes from the cotangent Laplacian of \[Section]2 applied to the vertex \
positions: the ", ital["mean-curvature normal"], " satisfies \[CapitalDelta]",
  ital["x"], " = 2", ital["H"], " ", ital["n"], " (Meyer et al. 2003). Below, \
the torus is coloured by ", ital["K"], " (left) and ", ital["H"], " (right) \
\[LongDash] both are live, so rotate them to see the warm positive-curvature \
outer rim and the cool inner rim:"}],
liveRow[{"torus_K", "torus_H"}, 300],
rotNote,
para[{"Quantitatively the discrete ", ital["K"], " matches the analytic torus \
curvature to relative L\.b2 error 5\[Times]10", e["\:207b\:2074"],
  " and ", ital["H"], " to 3\[Times]10", e["\:207b\:2074"],
  "; the unit sphere's ", ital["H"], " = 1 is recovered to 5\[Times]10", e["\:207b\.b3"],
  ". The convergence rates are quantified in \[Section]7."}],

(* ====================== 2. Cotan Laplacian ======================= *)
hd1["2.  The cotangent Laplace\[Dash]Beltrami operator"],
para[{"The Laplace\[Dash]Beltrami operator \[CapitalDelta] = div grad is the \
engine of the smooth theory. Its discrete form is the finite-element ",
  ital["stiffness matrix"], " of the hat functions: entry (", ital["i"], ",",
  ital["j"], ") is the integral of ", e[RowBox[{"\[Del]", SubscriptBox["\[Phi]", "i"],
    "\[CenterDot]", "\[Del]", SubscriptBox["\[Phi]", "j"]}]], " over the surface. \
Carrying out that integral over each triangle yields the famous cotangent \
weights,"}],
dm[HoldForm[Subscript[(L f), i] ==
   (1/2) Sum[(Cot[Subscript[\[Alpha], ij]] + Cot[Subscript[\[Beta], ij]])
      (Subscript[f, j] - Subscript[f, i]), j]]],
para[{"with ", e[SubscriptBox["\[Alpha]", "ij"]], ", ", e[SubscriptBox["\[Beta]", "ij"]],
  " the angles opposite edge (", ital["i"], ",", ital["j"], "). Three properties \
make it the right operator, all holding by construction: it is ", bold["symmetric"],
  " (real spectrum); it kills constants, ", ital["L"], " ", ital["1"],
  " = 0 (\"\[CapitalDelta] of a constant is zero\"); and it is ", bold[
  "negative semi-definite"], " and ", bold["local"], ". Paired with the diagonal ",
  bold["mass matrix"], " ", ital["M"], " of vertex areas, ", ital["M"], e["\:207b\.b9"],
  ital["L"], " is the discrete \[CapitalDelta]. Every later section is this one \
matrix with a different right-hand side."}],
wlIn["L = DDG`cotanLaplacian[mesh];          (* V\[Times]V SparseArray *)
Max[Abs[L - Transpose[L]]]                 (* 0  : symmetric *)
Max[Abs[L . ConstantArray[1., Length[L]]]] (* ~1e-15 : constants in kernel *)
M = DDG`massMatrix[mesh];                   (* diagonal vertex areas *)"],
para[{"In \[Section]6 the very same matrix re-emerges from discrete exterior \
calculus as ", ital["L"], " = ", e[SubscriptBox["d", "0"]], e["\[Transpose]"], " ",
  e[SubscriptBox["\[Star]", "1"]], " ", e[SubscriptBox["d", "0"]],
  " \[LongDash] two independent derivations of one object."}],

(* ====================== 3. Spectral ============================== *)
hd1["3.  Spectral geometry: hearing the shape of a mesh"],
para[{"Kac (1966) asked: ", ital["can one hear the shape of a drum?"],
  " The eigenpairs of \[CapitalDelta] are a surface's natural frequencies and \
standing waves. We solve the generalized eigenproblem"}],
dm[HoldForm[-L . \[Phi] == \[Lambda] M . \[Phi]]],
para[{"symmetrised as ", e[RowBox[{SuperscriptBox["M", RowBox[{"-", "1", "/", "2"}]],
    RowBox[{"(", RowBox[{"-", "L"}], ")"}], SuperscriptBox["M", RowBox[{"-", "1", "/", "2"}]]}]],
  ", positive semi-definite so the eigenvalues ", e[SubscriptBox["\[Lambda]", "k"]],
  " \[GreaterEqual] 0 are real. Two facts make the spectrum meaningful. First, on \
the unit sphere it is known exactly \[LongDash] the spherical harmonics satisfy ",
  e[ToBoxes[TraditionalForm[HoldForm[\[CapitalDelta] Subscript[Y, "lm"] ==
     -l (l + 1) Subscript[Y, "lm"]]]]], " \[LongDash] so the eigenvalues must be ",
  ital["l"], "(", ital["l"], "+1) with multiplicity 2", ital["l"], "+1:"}],
tableCell[Grid[{
   {Style["l", Bold], Style["l(l+1)", Bold], Style["discrete eigenvalues", Bold]},
   {0, 0, "0"}, {1, 2, "2.000, 2.000, 2.000"},
   {2, 6, "5.97, 5.97, 5.97, 5.97, 5.97"}, {3, 12, "11.80\[Times]3, 11.85\[Times]4"}},
  Frame -> All, FrameStyle -> GrayLevel[0.8], Spacings -> {2, 1.1},
  Background -> {None, {RGBColor[0.93, 0.95, 0.99], None}}, Alignment -> Left]],
para[{"Second, ", bold["Weyl's law"], " says the eigenvalue counting function \
grows like the area: ", e[ToBoxes[TraditionalForm[HoldForm[\[ScriptCapitalN][\[Lambda]] \[TildeTilde]
    (A/(4 \[Pi])) \[Lambda]]]]], ". So although you cannot fully hear a shape \
(isospectral non-isometric drums exist), you ", ital["can"], " hear its area \
\[LongDash] which is why the spectrum is used as an isometry-invariant ",
  ital["Shape-DNA"], " fingerprint (Reuter et al. 2006). The discrete \
eigenvectors are M-orthonormal to 10", e["\:207b\:00b9\:2075"],
  " and, on the sphere, are literally the spherical harmonics. Rotate one:"}],
liveCell["sphere_mode", 360],
rotNote,
imgCell["eigenfunctions.png", 520],
captionCell["Figure 1. The first nine non-constant eigenfunctions on a geodesic \
sphere \[LongDash] the discrete spherical harmonics for ", ital["l"], " = 1, 2, 3."],
para[{"Because the eigenvectors are an orthonormal basis, truncating it low-pass \
filters geometry exactly as a Fourier series filters a signal:"}],
imgCell["spectral_smoothing.png", 620],
captionCell["Figure 2. Spectral mesh smoothing: a bumpy sphere reconstructed from \
its lowest ", ital["K"], " Laplacian eigenmodes \[LongDash] geometry's Fourier \
compression."],

(* ====================== 4. Mean-curvature flow =================== *)
hd1["4.  Mean-curvature flow: smoothing as gradient descent of area"],
para[{"Surface area has a gradient, and ", bold["mean-curvature flow"],
  " is steepest descent along it: the first variation of area is ",
  e[ToBoxes[TraditionalForm[HoldForm[D[x, t] == \[CapitalDelta] x == -2 H n]]]],
  ", so each point slides along its \
mean-curvature normal and the surface shrinks as fast as possible while \
smoothing. (Huisken 1984: a convex surface stays convex and collapses to a round \
point.) Discretising the time step two ways is instructive. The ", ital["explicit"],
  " step is stable only when ", e[ToBoxes[TraditionalForm[HoldForm[\[Tau] <
     Superscript[h, 2]/(2 Subscript[\[Lambda], "max"])]]]], " (a CFL condition, \
forcing thousands of tiny steps); the ", ital["semi-implicit"], " step (Desbrun, \
Meyer, Schr\[ODoubleDot]der & Barr 1999) is unconditionally stable at one sparse \
solve per step:"}],
dm[HoldForm[(M - \[Tau] L) . Superscript[x, "n+1"] == M . Superscript[x, "n"]]],
para[{"A clean quantitative test: a round sphere stays round with ", ital["d"],
  "(", ital["R"], e["\.b2"], ")/", ital["dt"], " = \[Minus]4, i.e. ",
  e[ToBoxes[TraditionalForm[HoldForm[Superscript[R, 2](t) ==
    Superscript[Subscript[R, 0], 2] - 4 t]]]], "; the solver recovers the slope \
\[Minus]3.98. Applied to a bumpy sphere and rescaled to constant volume (so the \
eye sees smoothing, not the shrinking bare flow also produces), the surface \
relaxes to round while its curvature equalises. Rotate the before/after pair:"}],
liveRow[{"mcf_bumpy", "mcf_faired"}, 290],
rotNote,
animCell["mcf_flow.gif"],
captionCell["Animation. The flow over time; the mean-curvature heat-map settles \
to uniform as the shape rounds."],

(* ====================== 5. Geodesics ============================= *)
hd1["5.  Geodesic distance by the heat method"],
para[{"Geodesic distance solves the eikonal equation |\[Del]", ital["\[Phi]"],
  "| = 1, nonlinear and awkward on a mesh. Varadhan's (1967) formula gives a \
linear route: for short time the heat kernel ", e[SubscriptBox["u", "t"]],
  " encodes distance through ", e[ToBoxes[TraditionalForm[HoldForm[\[Phi] ==
    Limit[Sqrt[-4 t Log[Subscript[u, t]]], t -> 0]]]]],
  ". Crane, Weischedel & Wardetzky (2013) turn this into three linear solves, \
each using only ", ital["L"], " and ", ital["M"], ": (1) diffuse heat from the \
source, ", e[ToBoxes[TraditionalForm[HoldForm[(M - t L) . u == \[Delta]]]]],
  "; (2) take the normalised, negated gradient ", e[ToBoxes[TraditionalForm[
    HoldForm[X == -Grad[u]/Norm[Grad[u]]]]]], ", a unit field pointing away from \
the source; (3) recover distance by one Poisson solve ", e[ToBoxes[
    TraditionalForm[HoldForm[L . \[Phi] == Div[X]]]]], "."}],
para[{"On the unit sphere the distance from the pole is the colatitude \
arccos(", ital["z"], "); the method matches it and converges under refinement \
(\[Section]7). The level sets are the ", ital["wavefronts"], "; where they \
collide is the ", bold["cut locus"], " (the antipode on a sphere, a richer set \
on the torus) \[LongDash] the points reachable by more than one shortest path, \
where the distance is non-smooth. Rotate the fields and follow a band around the \
torus' hole:"}],
liveRow[{"geo_sphere", "geo_torus"}, 300],
rotNote,

(* ====================== 6. DEC + Hodge =========================== *)
hd1["6.  Discrete exterior calculus and the Hodge decomposition"],
para[{"Differential forms unify gradient, curl and divergence. On a mesh a ",
  ital["k"], "-form is a number per vertex (0-form), edge (1-form) or face \
(2-form), and the exterior derivative ", ital["d"], " becomes signed incidence: ",
  e[SubscriptBox["d", "0"]], " (vertices \[Rule] edges) is the discrete gradient, ",
  e[SubscriptBox["d", "1"]], " (edges \[Rule] faces) the discrete curl. The \
identity ", ital["d"], e["\.b2"], " = 0 holds combinatorially \[LongDash] the \
boundary of a boundary is empty:"}],
dm[HoldForm[Subscript[d, 1] . Subscript[d, 0] == 0]],
para[{"With diagonal Hodge stars (", e[SubscriptBox["\[Star]", "0"]],
  " vertex areas, ", e[SubscriptBox["\[Star]", "1"]], " cotan edge weights, ",
  e[SubscriptBox["\[Star]", "2"]], " inverse face areas) the Laplacian of \
\[Section]2 reappears, ", ital["L"], " = ", e[SubscriptBox["d", "0"]], e["\[Transpose]"],
  " ", e[SubscriptBox["\[Star]", "1"]], " ", e[SubscriptBox["d", "0"]],
  " (verified equal to ", mono["cotanLaplacian"], " at machine zero). The payoff \
is a complete splitting of any 1-form:"}],
thmBox["Hodge\[Dash]Helmholtz / de Rham.",
  {"Every 1-form splits uniquely and ", e[SubscriptBox["\[Star]", "1"]],
   "-orthogonally as ", e[ToBoxes[TraditionalForm[HoldForm[\[Omega] ==
      d \[Alpha] + \[Delta] \[Beta] + \[Gamma]]]]], " (exact + coexact + \
harmonic). By Hodge's theorem the harmonic space is isomorphic to the first de \
Rham cohomology, so its dimension is ", e[ToBoxes[TraditionalForm[HoldForm[
      Subscript[b, 1] == 2 g]]]], " (g = genus): the harmonic forms ", ital["count \
the handles"], "."}],
para[{"Computed as ", e[SubscriptBox["b", "1"]], " = E \[Minus] rank ",
  e[SubscriptBox["d", "0"]], " \[Minus] rank ", e[SubscriptBox["d", "1"]],
  ", the discrete harmonic dimension is exactly 2 on the torus and 0 on the \
sphere. The torus' two generators circulate around its two cycles (the meridian \
and the longitude). Decomposing a swirl, the harmonic part is the dominant \
circulation a gradient or curl cannot produce \[LongDash] rotate it:"}],
liveCell["hodge_harmonic", 440],
rotNote,
imgCell["hodge_decomposition.png", 640],
captionCell["Figure 3. The full Hodge decomposition of the swirl: exact \
(gradient, \[TildeTilde]0%), coexact (curl, 27%), harmonic (73%). The harmonic \
component is the topological circulation around the hole."],

(* ====================== 7. Convergence =========================== *)
hd1["7.  Convergence and the order of accuracy"],
para[{"\"Matches the smooth answer\" is only half the story; the scientific \
question is ", ital["how fast"], " the error falls as the mesh spacing ", ital["h"],
  " shrinks. Refining our test surfaces and fitting ", ital["error"], " ~ ",
  ital["C"], " ", ital["h"], e[SuperscriptBox["", "p"]],
  " by log-log least squares gives the empirical order ", ital["p"], ":"}],
imgCell["convergence.png", 620],
captionCell["Figure 4. Order of accuracy. Curvature is second order \
(", ital["p"], " \[TildeTilde] 2, on the slope-2 guide); the heat method is \
sub-linear (", ital["p"], " \[TildeTilde] 0.6); and the Gauss\[Dash]Bonnet \
residual sits at machine \[Epsilon] (\[TildeTilde]10", e["\:207b\:00b9\.b2"],
  ") at every resolution."],
para[{"Two lessons. (i) The pointwise operators are second-order accurate \
\[LongDash] the same order as piecewise-linear finite elements, which is exactly \
what they are. (ii) The Gauss\[Dash]Bonnet residual does ", ital["not"], " \
improve with refinement because it never had error to begin with: it is a \
combinatorial identity. Structure-preservation and approximation accuracy are \
different virtues, and DDG delivers both \[LongDash] exact where a theorem \
demands it, convergent where a number is estimated."}],

(* ====================== 8. Real scan ============================= *)
hd1["8.  The operators on a real 3D scan"],
para[{"Spheres and tori let us check answers; real geometry shows the operators \
are not toys. We take a 503k-vertex scanned ", bold["Yoda"], " figurine (from the \
Wolfram3DObjects collection), weld its texture seams, keep the largest component, \
and quadric-decimate it to a clean 12k-triangle mesh (a one-off ", mono["open3d"],
  " step; the result is committed, so the analysis stays pure Wolfram Language). \
Every operator runs on it unchanged. Rotate the geodesic field \[LongDash] note \
the distance creeping ", ital["along"], " the staff, reaching the tip only after \
travelling down the arm, exactly as an ant on the surface would:"}],
liveCell["yoda_geo", 420],
rotNote,
para[{"And mean curvature picks out the folds of the robe and the sharp creases \
(a robust quantile clip suppresses scan noise):"}],
liveCell["yoda_H", 420],
rotNote,
imgCell["yoda_eigenfunctions.png", 560],
captionCell["Figure 5. Laplace\[Dash]Beltrami eigenfunctions on the scan: the \
slender staff carries a distinct low-frequency mode; spatial frequency rises \
with the eigenvalue. The DDG operators degrade gracefully on messy, \
boundary-carrying geometry \[LongDash] which is why they underpin real \
geometry-processing software."],

(* ====================== 9. Graphs ================================ *)
hd1["9.  From meshes to networks: discrete curvature on graphs"],
para[{"A triangle mesh is, underneath, just a ", bold["graph"],
  " \[LongDash] vertices joined by edges. Strip away the coordinates and ask: \
how much geometry survives in the bare combinatorics? This is the question of \
network science and of the ", bold["Wolfram Physics Project"], ", where space \
itself is meant to ", ital["emerge"], " from a graph. Remarkably, both curvature \
and dimension can be read off connectivity alone."}],
para[{"The cleanest notion of graph curvature is ", bold["Ollivier-Ricci"], ". \
Put a probability measure ", e[SubscriptBox["m", "x"]], " on each vertex's \
neighbours and compare neighbouring vertices by ", ital["optimal transport"],
  ": the curvature of edge (", ital["x"], ",", ital["y"], ") is"}],
dm[HoldForm[\[Kappa][x, y] == 1 - Subscript[W, 1][Subscript[m, x], Subscript[m, y]]/d[x, y]]],
para[{"where ", e[SubscriptBox["W", "1"]], " is the Wasserstein-1 (earth-mover) \
distance and ", ital["d"], " the graph distance. The intuition is Ricci \
curvature's own: if neighbourhoods are ", ital["closer"], " than their centres \
(overlapping, easy to transport) the curvature is positive; if ", ital["farther"],
  " (as at a tree's branch points) it is negative; flat if equal. We solve the \
transport as a small linear program. The signs come out exactly as geometry \
predicts:"}],
tableCell[Grid[{
   {Style["graph", Bold], Style["mean Ollivier-Ricci", Bold], Style["geometry", Bold]},
   {"complete K\:2086", "+0.80", "very positively curved"},
   {"cycle C\:2088", "0.00", "flat (1-dimensional)"},
   {"6\[Times]6 grid", "0.00", "flat (2-dimensional)"},
   {"binary tree", "\[Minus]0.21", "negatively curved"}},
  Frame -> All, FrameStyle -> GrayLevel[0.8], Spacings -> {2, 1.1},
  Background -> {None, {RGBColor[0.93, 0.95, 0.99], None}}, Alignment -> Left]],
para[{"Now point this at our own meshes, ", ital["as graphs"],
  ". A geodesic sphere is almost a hexagonal tiling \[LongDash] but Euler's \
formula forbids tiling a sphere with hexagons alone. Exactly ", bold["twelve"],
  " pentagons (degree-5 vertices) are forced, and that is precisely where the \
positive Ollivier-Ricci curvature concentrates: the ", ital["soccer-ball \
theorem"], ", a combinatorial face of Gauss\[Dash]Bonnet (\[Chi] = 2). Rotate the \
sphere to find its twelve defects:"}],
liveCell["soccer", 400],
rotNote,
para[{"The structured torus, by contrast, is a ", ital["perfect"],
  " triangular lattice \[LongDash] every vertex has degree 6 \[LongDash] so its \
Ollivier-Ricci curvature is zero everywhere, the combinatorial signature of an \
intrinsically flat surface (\[Chi] = 0). The graph cannot see the embedding's \
bending, only its intrinsic, topological curvature."}],
para[{"Dimension, too, is emergent. In ", ital["d"], "-dimensional space a ball \
of radius ", ital["r"], " holds \[TildeTilde]", ital["r"], e[SuperscriptBox["", "d"]],
  " points, so growing geodesic balls on the graph and fitting"}],
dm[HoldForm[Abs[Subscript[B, r]] \[Tilde] Superscript[r, d] \[DoubleLongRightArrow]
   d == Limit[Log[Abs[Subscript[B, r]]]/Log[r], r -> \[Infinity]]]],
para[{"recovers the dimension. On periodic lattices it returns 1, 2, 3 (with a \
characteristic finite-size underestimate from the discrete ball corrections), \
and our sphere and torus mesh-graphs both come out two-dimensional \[LongDash] \
read purely from their connectivity, with no coordinates at all:"}],
imgCell["graph_dimension.png", 600],
captionCell["Figure 6. Emergent dimension from ball growth. The log-log slopes \
separate 1D (ring), 2D (2-torus and both mesh-graphs) and 3D (3-torus)."],

(* ====================== 10. Wolfram Physics ====================== *)
hd1["10.  A Wolfram-Physics bridge: emergent geometry of a hypergraph"],
para[{"The Wolfram model evolves a ", bold["hypergraph"],
  " by a rewriting rule, applied everywhere it matches, over and over. The hope \
of the Wolfram Physics Project is that physical space \[LongDash] continuous, \
three-dimensional, gently curved \[LongDash] emerges from the large-scale \
structure of such a graph. Here is one hypergraph after a few generations of the \
rule ", e[ToBoxes[TraditionalForm[HoldForm[{{1, 2, 3}} ->
     {{1, 2, 4}, {4, 3, 5}, {5, 2, 1}}]]]], ":"}],
imgCell["wm_hypergraph.png", 380],
captionCell["Figure 7. A Wolfram-model hypergraph (the spatial graph of a \
rewriting system)."],
para[{"We can interrogate its geometry with the very same tools. Its geodesic \
balls grow ", ital["faster"], " than any power of ", ital["r"],
  " (no finite dimension), and its Ollivier-Ricci curvature is negative on \
average (mean \[TildeTilde] \[Minus]0.13) with strongly negative spikes (to \
\[Minus]1.5) at the high-degree hubs: this rule grows a ", bold["negatively \
curved, tree-like"], " emergent space \[LongDash] the opposite of the flat \
lattices and the two-dimensional mesh-graphs above."}],
imgCell["wm_curvature.png", 520],
captionCell["Figure 8. The Wolfram-model spatial graph coloured by Ollivier-Ricci \
curvature (blue = negative): a negatively curved, tree-like emergent geometry."],
hd2["A gallery of networks"],
para[{"The two probes \[LongDash] curvature \[Kappa] and dimension ", ital["d"],
  " \[LongDash] are a general lens, so we turn them on a small zoo of networks. \
Each is laid out, then placed in the ", ital["curvature-dimension plane"], ":"}],
imgCell["network_gallery.png", 620],
captionCell["Figure 9. A geometric zoo: periodic lattices, a random geometric \
graph, a small-world (Watts\[Dash]Strogatz) and a scale-free (Barab\[AAcute]si\
\[Dash]Albert) network, and a Wolfram-model hypergraph, each labelled with its \
estimated dimension and mean Ollivier-Ricci curvature."],
imgCell["network_plane.png", 580],
captionCell["Figure 10. The curvature-dimension plane. The \[Kappa] = 0 line is \
the robust classifier: lattices sit on it (Euclidean), the clustered random \
geometric graph rises above it (positively curved), and the small-world, \
scale-free and Wolfram-model networks fall below (negatively curved). Ball-growth \
dimension is reliable for the regular lattices and only indicative for the \
small-world and dense graphs, whose balls saturate too fast for a clean power law."],
para[{"Two robust lessons survive the noise in ", ital["d"], ". First, ",
  bold["curvature is the cleaner classifier"], ": it cleanly separates flat \
(\[Kappa]=0), clustered/positively-curved, and hub-dominated/negatively-curved \
networks. Second, the networks that look ", ital["geometric"],
  " (lattices, the random geometric graph) are precisely those near \[Kappa]=0 \
with an integer-ish dimension; ", ital["small-world"], " structure \[LongDash] \
shortcuts, hubs \[LongDash] shows up as negative curvature and a runaway, \
ill-defined dimension. This is exactly the diagnostic the search for a \
physically realistic Wolfram-model rule needs."}],
thmBox["The open problem.",
  {"Most simple rules give either 1-dimensional or exponentially-growing \
(hyperbolic) space. Finding rules whose large-scale limit is flat and \
finite-dimensional \[LongDash] like the 3+1-dimensional space we live in \
\[LongDash] is the central challenge of the programme. The discrete-geometry \
toolkit of this notebook is exactly what one needs to test a candidate rule."}],

(* ====================== 11. Validation =========================== *)
hd1["11.  Validation summary"],
para[{"Nothing above is asserted without a check against a smooth reference, a \
conservation law, or a topological invariant:"}],
tableCell[Grid[{
   {Style["quantity", Bold], Style["test", Bold], Style["result", Bold]},
   {"Gaussian curvature", "vs analytic torus K; order", "5\[Times]10\:207b\:2074 ; p=1.99"},
   {"Gauss\[Dash]Bonnet", "sphere total = 4\[Pi]", "residual 10\:207b\:00b9\.b2 (identity)"},
   {"mean curvature", "sphere H = 1; order", "5\[Times]10\:207b\.b3 ; p=2.00"},
   {"LBO spectrum", "sphere \[Lambda] = l(l+1)", "rel err 6\[Times]10\:207b\.b3"},
   {"eigenvectors", "M-orthonormality", "10\:207b\:00b9\:2075"},
   {"mean-curvature flow", "d(R\.b2)/dt = \[Minus]4", "\[Minus]3.98"},
   {"heat geodesics", "vs arccos(z); order", "1.3%\[Rule]0.67% ; p=0.59"},
   {"DEC", "d\.b2=0 ; L=d\:2080\[Transpose]\[Star]\:2081 d\:2080", "exact (0)"},
   {"Hodge", "harmonic dim = 2g", "torus 2, sphere 0"},
   {"Ollivier-Ricci", "K\:2086>0, cycle/grid=0, tree<0", "signs correct"},
   {"emergent dimension", "periodic lattices", "1, 2, 3 recovered"},
   {"soccer-ball theorem", "sphere degree-5 defects", "exactly 12"}},
  Frame -> All, FrameStyle -> GrayLevel[0.8], Spacings -> {2, 1.1},
  Background -> {None, {RGBColor[0.93, 0.95, 0.99], None}}, Alignment -> Left]],

(* ====================== 12. References =========================== *)
hd1["12.  References"],
ref["Meyer, M., Desbrun, M., Schr\[ODoubleDot]der, P., Barr, A. (2003). ",
  ital["Discrete differential-geometry operators for triangulated 2-manifolds."],
  " Visualization and Mathematics III, 35\[Dash]57."],
ref["Desbrun, M., Meyer, M., Schr\[ODoubleDot]der, P., Barr, A. (1999). ",
  ital["Implicit fairing of irregular meshes using diffusion and curvature flow."],
  " SIGGRAPH '99, 317\[Dash]324."],
ref["Crane, K., Weischedel, C., Wardetzky, M. (2013). ", ital["Geodesics in heat."],
  " ACM Trans. Graph. 32(5). ", link["cs.cmu.edu/~kmcrane/Projects/HeatMethod",
    "https://www.cs.cmu.edu/~kmcrane/Projects/HeatMethod/"]],
ref["Reuter, M., Wolter, F.-E., Peinecke, N. (2006). ", ital["Laplace\[Dash]Beltrami \
spectra as Shape-DNA of surfaces and solids."], " Computer-Aided Design 38, 342\[Dash]366."],
ref["Huisken, G. (1984). ", ital["Flow by mean curvature of convex surfaces into \
spheres."], " J. Differential Geom. 20, 237\[Dash]266."],
ref["Crane, K. ", ital["Discrete Differential Geometry: An Applied Introduction"],
  " (CMU 15-458/858). ", link["cs.cmu.edu/~kmcrane/Projects/DDG",
    "https://www.cs.cmu.edu/~kmcrane/Projects/DDG/"]],
ref["Botsch, M., Kobbelt, L., Pauly, M., Alliez, P., L\[EAcute]vy, B. (2010). ",
  ital["Polygon Mesh Processing."], " AK Peters."],
ref["Ollivier, Y. (2009). ", ital["Ricci curvature of Markov chains on metric \
spaces."], " J. Functional Analysis 256, 810\[Dash]864."],
ref["Wolfram, S. (2020). ", ital["A Project to Find the Fundamental Theory of \
Physics."], " Wolfram Media. ", link["wolframphysics.org", "https://www.wolframphysics.org/"]],

(* ====================== 13. Reproducibility ====================== *)
hd1["13.  Reproducibility"],
para[{"All code is pure Wolfram Language. The package ", mono["wolfram/ddg.wl"],
  " defines the mesh accessors, test meshes and operators; each section's \
figures come from one standalone script, and every operator has a sanity check \
in ", mono["tests/"], " (all exit 0). ", mono["wolfram/models.wls"],
  " exports the live Graphics3D in this notebook; the one non-WL step is a \
one-off ", mono["open3d"], " decimation of the Yoda scan."}],
codeIn["wolframscript -file wolfram/curvature.wls     # \[Section]1  Gaussian curvature
wolframscript -file wolfram/meancurvature.wls # \[Section]1-2 mean curvature
wolframscript -file wolfram/spectral.wls      # \[Section]3  spectrum + harmonics
wolframscript -file wolfram/meanflow.wls      # \[Section]4  mean-curvature flow
wolframscript -file wolfram/heat.wls          # \[Section]5  heat-method geodesics
wolframscript -file wolfram/hodge.wls         # \[Section]6  DEC + Hodge
wolframscript -file wolfram/convergence.wls   # \[Section]7  order of accuracy
wolframscript -file wolfram/yoda.wls          # \[Section]8  the 3D scan
wolframscript -file wolfram/graphcurvature.wls # \[Section]9  graph curvature + dimension
wolframscript -file wolfram/wolframmodel.wls  # \[Section]10 Wolfram-model hypergraph
wolframscript -file wolfram/models.wls        # live rotatable models
wolframscript -file community/build_notebook.wls   # this notebook
# wolfram/run_all.wls runs the whole pipeline end to end."],
para[{Style["Built " <> DateString[{"MonthName", " ", "DayShort", ", ", "Year"}] <>
   " with Wolfram Language. Repository scaffolding and notebook build assisted by \
Claude Code.", FontSlant -> Italic, FontColor -> GrayLevel[0.45]]}]

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
