(* ::Package:: *)

(* ddg.wl — shared discrete-differential-geometry helpers for triangle meshes.
   Loaded by the wolfram/*.wls exploration scripts.

   A "mesh" here is a Wolfram MeshRegion whose 2-cells are triangles, OR is
   built directly from {coordinates, triangleIndexLists}. All operators take a
   MeshRegion and return per-vertex lists aligned with MeshCoordinates[mr].   *)

BeginPackage["DDG`"];

triMesh::usage           = "triMesh[coords, tris] builds a triangle MeshRegion from a coordinate list and a list of {i,j,k} index triples (1-based).";
meshCoords::usage        = "meshCoords[mr] returns the vertex coordinate list.";
meshTriangles::usage     = "meshTriangles[mr] returns the triangle vertex-index triples (1-based).";
vertexAreasBary::usage   = "vertexAreasBary[mr] returns the barycentric (1/3 of incident triangle area) vertex areas.";
angleDefect::usage       = "angleDefect[mr] returns the per-vertex angle defect 2*Pi - sum of incident interior angles (integrated Gaussian curvature for interior vertices).";
gaussianCurvature::usage = "gaussianCurvature[mr] returns the pointwise Gaussian curvature angleDefect/vertexArea.";
gaussBonnetCheck::usage  = "gaussBonnetCheck[mr] returns <|\"totalDefect\"->.., \"twoPiChi\"->.., \"chi\"->.., \"residual\"->..|> for a closed mesh.";

Begin["`Private`"];

triMesh[coords_, tris_] := MeshRegion[N[coords], Polygon[tris]];

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

gaussBonnetCheck[mr_MeshRegion] := Module[{defect, chi, twoPiChi},
  defect = Total[angleDefect[mr]];
  chi = EulerCharacteristic[mr];
  twoPiChi = 2. Pi chi;
  <|"totalDefect" -> defect, "chi" -> chi, "twoPiChi" -> twoPiChi,
    "residual" -> defect - twoPiChi|>];

End[];
EndPackage[];
