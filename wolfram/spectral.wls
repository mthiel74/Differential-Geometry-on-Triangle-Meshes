#!/usr/bin/env wolframscript
(* spectral.wls — spectral geometry of the Laplace-Beltrami operator.

   The cotan-Laplacian's eigenpairs (-L) phi = lambda M phi are a mesh's
   "harmonics": eigenvalues are a shape fingerprint, eigenvectors are smooth
   standing waves.

   1. Validation: on a unit sphere the eigenvalues must be l(l+1) =
      0,2,6,12,20,... with multiplicity 2l+1. We recover this.
   2. Eigenfunction gallery: the low modes on the sphere are the spherical
      harmonics -> docs/images/eigenfunctions.png.
   3. Spectral mesh smoothing: reconstruct a bumpy mesh from its lowest K
      manifold-harmonic modes (the geometry analogue of a Fourier low-pass)
      -> docs/images/spectral_smoothing.png.                                *)

Needs["DDG`", FileNameJoin[{DirectoryName[$InputFileName], "ddg.wl"}]];

imgDir = FileNameJoin[{ParentDirectory[DirectoryName[$InputFileName]], "docs", "images"}];

(* ---- 1. sphere spectrum vs l(l+1) -------------------------------------- *)
sphere = DDG`icosphereMesh[3];
nV = Length[DDG`meshCoords[sphere]];
{vals, vecs} = DDG`laplacianSpectrum[sphere, 17];
Print["LBO spectrum on icosphere(3), ", nV, " vertices:"];
Print["  l   exact l(l+1)   discrete eigenvalues (multiplicity 2l+1)"];
fmt[x_] := ToString[NumberForm[Chop[N[x], 1.*^-6], {6, 3}]];
Do[
  With[{lo = l^2 + 1, hi = (l + 1)^2},      (* 1-based index block for degree l *)
   Print["  ", l, "       ", l (l + 1), "         ",
     StringRiffle[fmt /@ vals[[lo ;; Min[hi, Length[vals]]]], "  "]]],
  {l, 0, 3}];

(* ---- 2. eigenfunction gallery (spherical harmonics) -------------------- *)
spts = DDG`meshCoords[sphere]; stris = DDG`meshTriangles[sphere];
modeFig[phi_, lbl_] := Module[{cmax = Max[Abs[phi]]},
  Graphics3D[{EdgeForm[],
     GraphicsComplex[spts, Polygon[stris],
       VertexColors -> Map[ColorData["BlueGreenYellow"],
          Rescale[phi, {-cmax, cmax}]]]},
    Boxed -> False, SphericalRegion -> True, ImageSize -> 260,
    ViewPoint -> {1.5, -2., 1.2}, Lighting -> "Neutral",
    PlotLabel -> Style[lbl, 11]]];

gallery = Table[
   modeFig[vecs[[j]], "\[Phi]" <> ToString[j - 1] <> "  \[Lambda]=" <>
      ToString[NumberForm[vals[[j]], {4, 2}]]],
   {j, 2, 10}];                              (* skip the constant phi0 *)
gfig = FileNameJoin[{imgDir, "eigenfunctions.png"}];
Export[gfig, Grid[Partition[gallery, 3], Spacings -> {0, 0}], ImageResolution -> 110];
Print["wrote ", gfig];

(* ---- 3. spectral mesh smoothing (manifold-harmonic low-pass) ----------- *)
SeedRandom[7];
bumpy = ((1. + 0.13 #[[3]]^2 + 0.10 Sin[3 #[[1]]] Cos[2 #[[2]]]
      + 0.05 RandomReal[{-1, 1}]) Normalize[#]) & /@ spts;
bmesh = DDG`triMesh[bumpy, stris];
nModes = 200;
{bvals, bvecs} = DDG`laplacianSpectrum[bmesh, nModes];
bM = DDG`massMatrix[bmesh];
coeffs = bvecs . (bM . bumpy);               (* nModes x 3 spectral coeffs *)
reconstruct[K_] := Transpose[bvecs[[1 ;; K]]] . coeffs[[1 ;; K]];

renderMesh[x_, lbl_] := Graphics3D[{EdgeForm[],
    GraphicsComplex[x, Polygon[stris], VertexColors ->
       Map[ColorData["BlueGreenYellow"], Rescale[x[[All, 3]]]]]},
   Boxed -> False, SphericalRegion -> True, ImageSize -> 260,
   ViewPoint -> {1.5, -2., 1.2}, Lighting -> "Neutral",
   PlotRange -> 1.3 {{-1, 1}, {-1, 1}, {-1, 1}},
   PlotLabel -> Style[lbl, 11]];

Ks = {3, 10, 40, nModes};
recon = Append[
   Table[renderMesh[reconstruct[K], "K = " <> ToString[K] <> " modes"], {K, Ks}],
   renderMesh[bumpy, "original"]];
recErr = Table[Norm[reconstruct[K] - bumpy]/Norm[bumpy], {K, Ks}];
Print["spectral reconstruction rel L2 error at K=", Ks, " : ",
   StringRiffle[fmt /@ recErr, "  "]];
sfig = FileNameJoin[{imgDir, "spectral_smoothing.png"}];
Export[sfig, Grid[{recon}, Spacings -> {0, 0}], ImageResolution -> 110];
Print["wrote ", sfig];
