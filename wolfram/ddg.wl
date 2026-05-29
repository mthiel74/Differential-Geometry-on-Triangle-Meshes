(* ::Package:: *)

(* ddg.wl — shared discrete-differential-geometry helpers for triangle meshes.
   Loaded by the wolfram/*.wls exploration scripts.

   A "mesh" here is a Wolfram MeshRegion whose 2-cells are triangles, OR is
   built directly from {coordinates, triangleIndexLists}. All operators take a
   MeshRegion and return per-vertex lists aligned with MeshCoordinates[mr].   *)

BeginPackage["DDG`"];

triMesh::usage           = "triMesh[coords, tris] builds a triangle MeshRegion from a coordinate list and a list of {i,j,k} index triples (1-based).";
torusMesh::usage         = "torusMesh[R, r, nu, nv] builds a closed (genus-1) torus triangle mesh from a periodic nu*nv (u,v) grid. R = ring radius, r = tube radius.";
torusAngleU::usage       = "torusAngleU[pts, R, r] recovers the tube angle u at each vertex of a torus mesh from its coordinates.";
icosphereMesh::usage     = "icosphereMesh[n] builds a regular geodesic sphere: an icosahedron subdivided n times and projected onto the unit sphere. Consistent outward winding, no pole degeneracy.";
meshCoords::usage        = "meshCoords[mr] returns the vertex coordinate list.";
meshTriangles::usage     = "meshTriangles[mr] returns the triangle vertex-index triples (1-based).";
vertexAreasBary::usage   = "vertexAreasBary[mr] returns the barycentric (1/3 of incident triangle area) vertex areas.";
angleDefect::usage       = "angleDefect[mr] returns the per-vertex angle defect 2*Pi - sum of incident interior angles (integrated Gaussian curvature for interior vertices).";
gaussianCurvature::usage = "gaussianCurvature[mr] returns the pointwise Gaussian curvature angleDefect/vertexArea.";
gaussBonnetCheck::usage  = "gaussBonnetCheck[mr] returns <|\"totalDefect\"->.., \"twoPiChi\"->.., \"chi\"->.., \"residual\"->..|> for a closed mesh.";
cotanLaplacian::usage    = "cotanLaplacian[mr] returns the V*V cotangent Laplace-Beltrami matrix L with (L f)_i = (1/2) sum_j (cot a_ij + cot b_ij)(f_j - f_i). Negative semi-definite.";
vertexNormals::usage     = "vertexNormals[mr] returns area-weighted unit vertex normals; orientation follows the triangle winding.";
meanCurvatureNormal::usage = "meanCurvatureNormal[mr] returns the per-vertex mean-curvature normal vector -(L.x)_i/(2 A_i) = kappaH n (Meyer et al. 2003).";
meanCurvature::usage     = "meanCurvature[mr] returns the signed per-vertex mean curvature (mean-curvature normal projected on the unit vertex normal).";
massMatrix::usage        = "massMatrix[mr] returns the diagonal (lumped) mass matrix of barycentric vertex areas.";
implicitFairing::usage   = "implicitFairing[coords, tris, tau, n] runs n steps of semi-implicit mean-curvature flow (Desbrun et al. 1999): (M - tau L) x' = M x. Returns the list of coordinate frames (length n+1).";
laplacianSpectrum::usage = "laplacianSpectrum[mr, k] returns {values, vectors} for the k smallest eigenpairs of the generalized problem (-L) phi = lambda M phi (Laplace-Beltrami). Eigenvectors are rows, M-orthonormal. Dense solver: use for meshes up to ~1000 vertices.";
faceGradient::usage      = "faceGradient[mr, u] returns the per-face gradient (one 3-vector per triangle) of a piecewise-linear vertex scalar u.";
vertexDivergence::usage  = "vertexDivergence[mr, X] returns the per-vertex integrated divergence of a per-face vector field X (cotangent formula).";
heatGeodesics::usage     = "heatGeodesics[mr, src] returns the per-vertex geodesic distance from vertex src by the heat method (Crane, Weischedel & Wardetzky 2013): diffuse, normalise the gradient, solve a Poisson problem.";
decOperators::usage      = "decOperators[mr] returns a discrete-exterior-calculus toolkit <|edges, d0, d1, star0, star1, star2|>: oriented edges, exterior derivatives d0 (V->E) and d1 (E->F), and diagonal Hodge stars.";
bettiOne::usage          = "bettiOne[mr] returns the first Betti number b1 = E - rank(d0) - rank(d1) = dim of the harmonic 1-forms (= 2*genus for a closed surface).";
hodgeDecomposition::usage = "hodgeDecomposition[mr, omega] splits an edge 1-form into <|exact, coexact, harmonic|> (Hodge-Helmholtz): omega = d0 alpha + delta beta + harmonic.";
oneFormToFaceField::usage = "oneFormToFaceField[mr, omega] reconstructs a per-face tangent vector from an edge 1-form (least-squares sharp), for visualization.";

Begin["`Private`"];

triMesh[coords_, tris_] := MeshRegion[N[coords], Polygon[tris]];

torusMesh[R_, r_, nu_, nv_] := Module[{us, vs, coords, idx, tris},
  us = N[2 Pi Range[0, nu - 1]/nu];
  vs = N[2 Pi Range[0, nv - 1]/nv];
  coords = Flatten[
    Table[{(R + r Cos[u]) Cos[v], (R + r Cos[u]) Sin[v], r Sin[u]},
      {u, us}, {v, vs}], 1];
  idx[i_, j_] := Mod[i, nu] nv + Mod[j, nv] + 1;          (* 1-based, periodic *)
  tris = Flatten[
    Table[{{idx[i, j], idx[i + 1, j], idx[i + 1, j + 1]},
           {idx[i, j], idx[i + 1, j + 1], idx[i, j + 1]}},
      {i, 0, nu - 1}, {j, 0, nv - 1}], 2];
  triMesh[coords, tris]];

torusAngleU[pts_, R_, r_] :=
  ArcTan[(Sqrt[pts[[All, 1]]^2 + pts[[All, 2]]^2] - R)/r, pts[[All, 3]]/r];

(* one 1-to-4 subdivision step on {coords, tris}, with midpoint de-duplication *)
subdivideOnce[{coords_, tris_}] := Module[{newCoords, mid, getMid, newTris},
  newCoords = coords; mid = <||>;
  getMid[a_, b_] := With[{key = Sort[{a, b}]},
    If[KeyExistsQ[mid, key], mid[key],
      AppendTo[newCoords, (coords[[a]] + coords[[b]])/2.];
      mid[key] = Length[newCoords]]];
  newTris = Flatten[
    Function[t,
      Module[{a = t[[1]], b = t[[2]], c = t[[3]], ab, bc, ca},
        ab = getMid[a, b]; bc = getMid[b, c]; ca = getMid[c, a];
        {{a, ab, ca}, {b, bc, ab}, {c, ca, bc}, {ab, bc, ca}}]] /@ tris, 1];
  {newCoords, newTris}];

icosphereMesh[n_Integer] := Module[{ch, coords, tris},
  ch = ConvexHullMesh[
    N[{{0, 1, GoldenRatio}, {0, -1, GoldenRatio}, {0, 1, -GoldenRatio},
       {0, -1, -GoldenRatio}, {1, GoldenRatio, 0}, {-1, GoldenRatio, 0},
       {1, -GoldenRatio, 0}, {-1, -GoldenRatio, 0}, {GoldenRatio, 0, 1},
       {-GoldenRatio, 0, 1}, {GoldenRatio, 0, -1}, {-GoldenRatio, 0, -1}}]];
  coords = MeshCoordinates[ch];
  tris = MeshCells[ch, 2][[All, 1]];
  Do[{coords, tris} = subdivideOnce[{coords, tris}], {n}];
  triMesh[Normalize /@ coords, tris]];

meshCoords[mr_MeshRegion] := MeshCoordinates[mr];

meshTriangles[mr_MeshRegion] := MeshCells[mr, 2][[All, 1]];

(* triangle area from three position vectors *)
triArea[a_, b_, c_] := 0.5 Norm[Cross[b - a, c - a]];

vertexAreasBary[mr_MeshRegion] := Module[{pts, tris, areas, acc},
  pts = meshCoords[mr];
  tris = meshTriangles[mr];
  areas = triArea[pts[[#1]], pts[[#2]], pts[[#3]]] & @@@ tris;
  acc = ConstantArray[0., Length[pts]];
  Do[
    With[{t = tris[[k]], w = areas[[k]]/3.},
      acc[[t[[1]]]] += w; acc[[t[[2]]]] += w; acc[[t[[3]]]] += w],
    {k, Length[tris]}];
  acc];

(* interior angle at vertex p0 of a triangle with the other two vertices p1,p2 *)
cornerAngle[p0_, p1_, p2_] := VectorAngle[p1 - p0, p2 - p0];

angleDefect[mr_MeshRegion] := Module[{pts, tris, acc},
  pts = meshCoords[mr];
  tris = meshTriangles[mr];
  acc = ConstantArray[2. Pi, Length[pts]];
  Do[
    With[{a = tris[[k, 1]], b = tris[[k, 2]], c = tris[[k, 3]]},
      acc[[a]] -= cornerAngle[pts[[a]], pts[[b]], pts[[c]]];
      acc[[b]] -= cornerAngle[pts[[b]], pts[[c]], pts[[a]]];
      acc[[c]] -= cornerAngle[pts[[c]], pts[[a]], pts[[b]]]],
    {k, Length[tris]}];
  acc];

gaussianCurvature[mr_MeshRegion] := angleDefect[mr]/vertexAreasBary[mr];

(* cotangent of the angle at the apex between edge vectors u, v *)
cotABetween[u_, v_] := (u . v)/Norm[Cross[u, v]];

cotanLaplacian[mr_MeshRegion] := Module[{pts, tris, rules, n, w, diag, rowsum},
  pts = meshCoords[mr]; tris = meshTriangles[mr]; n = Length[pts];
  (* for triangle (a,b,c): angle at c is opposite edge (a,b), contributes
     (1/2) cot(c) to off-diagonal entries (a,b) and (b,a). *)
  rules = Flatten[
    Function[t,
      Module[{a = t[[1]], b = t[[2]], c = t[[3]], pa, pb, pc, wc, wa, wb},
        pa = pts[[a]]; pb = pts[[b]]; pc = pts[[c]];
        wc = 0.5 cotABetween[pa - pc, pb - pc];
        wa = 0.5 cotABetween[pb - pa, pc - pa];
        wb = 0.5 cotABetween[pc - pb, pa - pb];
        {{a, b} -> wc, {b, a} -> wc,
         {b, c} -> wa, {c, b} -> wa,
         {c, a} -> wb, {a, c} -> wb}]] /@ tris, 1];
  w = SparseArray[Normal@Merge[rules, Total], {n, n}];   (* sum shared edges *)
  rowsum = Total[w, {2}];
  diag = SparseArray[{i_, i_} :> -rowsum[[i]], {n, n}];
  w + diag];

vertexNormals[mr_MeshRegion] := Module[{pts, tris, acc, fn},
  pts = meshCoords[mr]; tris = meshTriangles[mr];
  acc = ConstantArray[0., {Length[pts], 3}];
  Do[
    With[{t = tris[[k]]},
      fn = Cross[pts[[t[[2]]]] - pts[[t[[1]]]], pts[[t[[3]]]] - pts[[t[[1]]]]];
      acc[[t[[1]]]] += fn; acc[[t[[2]]]] += fn; acc[[t[[3]]]] += fn],
    {k, Length[tris]}];
  Normalize /@ acc];

meanCurvatureNormal[mr_MeshRegion] := Module[{l, a},
  l = cotanLaplacian[mr]; a = vertexAreasBary[mr];
  -(l . meshCoords[mr])/(2. a)];

meanCurvature[mr_MeshRegion] :=
  MapThread[Dot, {meanCurvatureNormal[mr], vertexNormals[mr]}];

massMatrix[mr_MeshRegion] :=
  SparseArray[Band[{1, 1}] -> vertexAreasBary[mr]];

(* Semi-implicit mean-curvature flow / implicit fairing (Desbrun, Meyer,
   Schroder, Barr 1999). The cotan operator L and mass M are rebuilt each
   step from the current geometry (they depend on it).                     *)
implicitFairing[coords0_, tris_, tau_, n_Integer] :=
  Module[{frames, x, mr, l, m, a, solver},
   x = N[coords0];
   frames = {x};
   Do[
     mr = triMesh[x, tris];
     l = cotanLaplacian[mr];
     m = massMatrix[mr];
     a = m - tau l;                 (* symmetric positive definite *)
     x = LinearSolve[a, m . x];      (* solves the 3 coordinate columns *)
     AppendTo[frames, x],
     {n}];
   frames];

(* Generalized eigenproblem (-L) phi = lambda M phi. With M diagonal lumped,
   symmetrize via S = M^-1/2 (-L) M^-1/2 (PSD), solve dense, map back. The
   eigenvectors are M-orthonormal because a_i (1/sqrt a_i)^2 = 1.            *)
laplacianSpectrum[mr_MeshRegion, k_Integer] := Module[
   {a, isq, lpos, s, vals, vecs, ord, dm},
   a = vertexAreasBary[mr];
   isq = 1./Sqrt[a];
   lpos = -cotanLaplacian[mr];
   dm = SparseArray[Band[{1, 1}] -> isq];
   s = dm . lpos . dm;                        (* symmetric PSD *)
   {vals, vecs} = Eigensystem[Symmetrize[N[Normal[s]]]];
   ord = Ordering[vals, k];                   (* k smallest *)
   vals = Clip[vals[[ord]], {0., Infinity}];   (* tiny negatives -> 0 *)
   vecs = (isq # &) /@ vecs[[ord]];            (* map psi -> phi = M^-1/2 psi *)
   {vals, vecs}];

(* ---- the heat method for geodesic distance (Crane et al. 2013) --------- *)
(* row-wise cross products / dots for F-by-3 arrays *)
crossRows[u_, v_] := u[[All, {2, 3, 1}]] v[[All, {3, 1, 2}]] -
   u[[All, {3, 1, 2}]] v[[All, {2, 3, 1}]];
dotRows[u_, v_] := Total[u v, {2}];
cotRows[u_, v_] := dotRows[u, v]/Sqrt[Total[crossRows[u, v]^2, {2}]];

faceGradient[mr_MeshRegion, u_] := Module[
   {pts, tris, pi, pj, pk, ui, uj, uk, nrm, a2},
   pts = meshCoords[mr]; tris = meshTriangles[mr];
   pi = pts[[tris[[All, 1]]]]; pj = pts[[tris[[All, 2]]]]; pk = pts[[tris[[All, 3]]]];
   ui = u[[tris[[All, 1]]]]; uj = u[[tris[[All, 2]]]]; uk = u[[tris[[All, 3]]]];
   nrm = crossRows[pj - pi, pk - pi];          (* = 2A * unit normal *)
   a2 = Sqrt[Total[nrm^2, {2}]];                (* = 2A *)
   nrm = nrm/a2;
   (* grad = (1/2A)(ui N x (pk-pj) + uj N x (pi-pk) + uk N x (pj-pi)) *)
   (ui crossRows[nrm, pk - pj] + uj crossRows[nrm, pi - pk] +
      uk crossRows[nrm, pj - pi])/a2];

vertexDivergence[mr_MeshRegion, x_] := Module[
   {pts, tris, pi, pj, pk, ci, cj, ck, di, dj, dk, n},
   pts = meshCoords[mr]; tris = meshTriangles[mr]; n = Length[pts];
   pi = pts[[tris[[All, 1]]]]; pj = pts[[tris[[All, 2]]]]; pk = pts[[tris[[All, 3]]]];
   ci = cotRows[pj - pi, pk - pi];              (* cot of angle at i, j, k *)
   cj = cotRows[pk - pj, pi - pj];
   ck = cotRows[pi - pk, pj - pk];
   (* contribution to each vertex of the face *)
   di = 0.5 (ck dotRows[pj - pi, x] + cj dotRows[pk - pi, x]);
   dj = 0.5 (ci dotRows[pk - pj, x] + ck dotRows[pi - pj, x]);
   dk = 0.5 (cj dotRows[pi - pk, x] + ci dotRows[pj - pk, x]);
   Lookup[Merge[Join[
       Thread[tris[[All, 1]] -> di], Thread[tris[[All, 2]] -> dj],
       Thread[tris[[All, 3]] -> dk]], Total], Range[n], 0.]];

meanEdgeLength[mr_MeshRegion] := Module[{pts, tris},
   pts = meshCoords[mr]; tris = meshTriangles[mr];
   Mean[Flatten[{
      Sqrt[Total[(pts[[tris[[All, 1]]]] - pts[[tris[[All, 2]]]])^2, {2}]],
      Sqrt[Total[(pts[[tris[[All, 2]]]] - pts[[tris[[All, 3]]]])^2, {2}]],
      Sqrt[Total[(pts[[tris[[All, 3]]]] - pts[[tris[[All, 1]]]])^2, {2}]]}]]];

heatGeodesics[mr_MeshRegion, src_Integer] := Module[
   {l, m, n, h, t, u, grad, gmag, x, div, phi},
   l = cotanLaplacian[mr]; m = massMatrix[mr]; n = Length[meshCoords[mr]];
   h = meanEdgeLength[mr]; t = h^2;
   (* 1. diffuse heat from the source: (M - t L) u = delta_src *)
   u = LinearSolve[m - t l, SparseArray[{src -> 1.}, n]];
   (* 2. unit vector field pointing away from the source *)
   grad = faceGradient[mr, u];
   gmag = Sqrt[Total[grad^2, {2}]] + 1.*^-300;
   x = -grad/gmag;
   (* 3. Poisson reconstruction (-L) phi = div X, lightly regularised *)
   div = vertexDivergence[mr, x];
   phi = LinearSolve[-l + 1.*^-8 m, div];
   phi = phi - phi[[src]];
   (* fix the global sign so distance increases away from the source *)
   If[Median[phi] < 0., -phi, phi]];

(* ---- discrete exterior calculus --------------------------------------- *)
decOperators[mr_MeshRegion] := Module[
   {pts, tris, nv, nf, edges, ne, eidx, d0, d1rules, s1, star0, star2, fa},
   pts = meshCoords[mr]; tris = meshTriangles[mr]; nv = Length[pts]; nf = Length[tris];
   edges = DeleteDuplicates[Sort /@ Flatten[
       ({{#[[1]], #[[2]]}, {#[[2]], #[[3]]}, {#[[3]], #[[1]]}}) & /@ tris, 1]];
   ne = Length[edges];
   eidx = Association[MapIndexed[#1 -> #2[[1]] &, edges]];
   (* d0: 1-forms = f_b - f_a on canonical edge a<b *)
   d0 = SparseArray[Flatten[
       Table[{{k, edges[[k, 1]]} -> -1., {k, edges[[k, 2]]} -> 1.}, {k, ne}], 1],
     {ne, nv}];
   (* d1 (signed face-edge incidence) and star1 (cotan edge weights),
      built without AppendTo *)
   d1rules = Flatten[Table[
       With[{a = tris[[f, 1]], b = tris[[f, 2]], c = tris[[f, 3]]},
        {{f, eidx[Sort[{a, b}]]} -> If[a < b, 1., -1.],
         {f, eidx[Sort[{b, c}]]} -> If[b < c, 1., -1.],
         {f, eidx[Sort[{c, a}]]} -> If[c < a, 1., -1.]}], {f, nf}], 1];
   s1 = Lookup[Merge[Flatten[Table[
        With[{a = tris[[f, 1]], b = tris[[f, 2]], c = tris[[f, 3]],
              pa = pts[[tris[[f, 1]]]], pb = pts[[tris[[f, 2]]]],
              pc = pts[[tris[[f, 3]]]]},
         {eidx[Sort[{a, b}]] -> 0.5 cotABetween[pa - pc, pb - pc],
          eidx[Sort[{b, c}]] -> 0.5 cotABetween[pb - pa, pc - pa],
          eidx[Sort[{c, a}]] -> 0.5 cotABetween[pc - pb, pa - pb]}], {f, nf}], 1],
      Total], Range[ne], 0.];
   fa = (triArea[pts[[#1]], pts[[#2]], pts[[#3]]] & @@@ tris);
   star0 = SparseArray[Band[{1, 1}] -> vertexAreasBary[mr]];
   star2 = SparseArray[Band[{1, 1}] -> 1./fa];
   <|"edges" -> edges, "d0" -> d0, "d1" -> SparseArray[d1rules, {nf, ne}],
     "star0" -> star0, "star1" -> SparseArray[Band[{1, 1}] -> s1],
     "star2" -> star2|>];

bettiOne[mr_MeshRegion] := Module[{dec = decOperators[mr]},
   Length[dec["edges"]] - MatrixRank[dec["d0"]] - MatrixRank[dec["d1"]]];

spId[n_] := SparseArray[Band[{1, 1}] -> 1., {n, n}];

hodgeDecomposition[mr_MeshRegion, omega_] := Module[
   {dec, d0, d1, s1, s2, s1inv, nv, nf, lap, alpha, exact, m2, beta, coexact, harmonic},
   dec = decOperators[mr];
   d0 = dec["d0"]; d1 = dec["d1"]; s1 = dec["star1"]; s2 = dec["star2"];
   (* floor near-zero star1 weights (right-angle edges) before inverting *)
   s1inv = SparseArray[Band[{1, 1}] ->
      1./(Normal[Diagonal[s1]] /. x_ /; Abs[x] < 1.*^-10 :> 1.*^-10)];
   nv = Dimensions[d0][[2]]; nf = Dimensions[d1][[1]];
   (* exact part d0 alpha: solve  d0^T s1 d0 alpha = d0^T s1 omega *)
   lap = Transpose[d0] . s1 . d0;
   alpha = LinearSolve[lap + 1.*^-8 spId[nv], Transpose[d0] . s1 . omega];
   exact = d0 . alpha;
   (* coexact part delta beta: solve d1 s1^-1 d1^T s2 beta = d1 omega *)
   m2 = d1 . s1inv . Transpose[d1] . s2;
   beta = LinearSolve[m2 + 1.*^-8 spId[nf], d1 . omega];
   coexact = s1inv . Transpose[d1] . s2 . beta;
   harmonic = omega - exact - coexact;
   <|"exact" -> exact, "coexact" -> coexact, "harmonic" -> harmonic,
     "edges" -> dec["edges"], "star1" -> s1|>];

oneFormToFaceField[mr_MeshRegion, omega_] := Module[
   {pts, tris, edges, eidx},
   pts = meshCoords[mr]; tris = meshTriangles[mr];
   edges = decOperators[mr]["edges"];
   eidx = Association[MapIndexed[#1 -> #2[[1]] &, edges]];
   (* per face: least-squares vector v with v.e = circulation on each edge *)
   Function[t,
     Module[{a = t[[1]], b = t[[2]], c = t[[3]], ev, cv},
      ev = {pts[[b]] - pts[[a]], pts[[c]] - pts[[b]], pts[[a]] - pts[[c]]};
      cv = {If[a < b, 1, -1] omega[[eidx[Sort[{a, b}]]]],
            If[b < c, 1, -1] omega[[eidx[Sort[{b, c}]]]],
            If[c < a, 1, -1] omega[[eidx[Sort[{c, a}]]]]};
      LeastSquares[ev, cv]]] /@ tris];

gaussBonnetCheck[mr_MeshRegion] := Module[{defect, chi, twoPiChi},
  defect = Total[angleDefect[mr]];
  chi = EulerCharacteristic[mr];
  twoPiChi = 2. Pi chi;
  <|"totalDefect" -> defect, "chi" -> chi, "twoPiChi" -> twoPiChi,
    "residual" -> defect - twoPiChi|>];

End[];
EndPackage[];
