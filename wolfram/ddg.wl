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

gaussBonnetCheck[mr_MeshRegion] := Module[{defect, chi, twoPiChi},
  defect = Total[angleDefect[mr]];
  chi = EulerCharacteristic[mr];
  twoPiChi = 2. Pi chi;
  <|"totalDefect" -> defect, "chi" -> chi, "twoPiChi" -> twoPiChi,
    "residual" -> defect - twoPiChi|>];

End[];
EndPackage[];
