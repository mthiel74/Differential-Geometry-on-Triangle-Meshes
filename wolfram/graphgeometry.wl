(* ::Package:: *)

(* graphgeometry.wl — discrete geometry on *graphs* (not just meshes): the
   curvature and dimension notions used in network science and the Wolfram
   Physics Project. This bridges the triangle-mesh DDG of ddg.wl to the
   purely combinatorial setting where space itself is a graph.

   - ollivierRicci : Ollivier-Ricci curvature of an edge via optimal transport
                     (Wasserstein-1 distance between neighbour distributions),
                     solved as a linear program.
   - formanRicci   : Forman-Ricci curvature (cheap, combinatorial).
   - graphDimension: emergent dimension from geodesic-ball growth |B_r| ~ r^d.
   - meshGraph     : the vertex-adjacency graph of a triangle mesh.            *)

BeginPackage["GraphDDG`"];

ollivierRicci::usage  = "ollivierRicci[g] returns <|edge -> curvature|> for every edge; ollivierRicci[g, {x,y}] one edge. Uniform neighbour measures, ground metric = graph distance.";
formanRicci::usage    = "formanRicci[g] returns <|edge -> curvature|> using the (triangle-augmented) Forman-Ricci formula.";
graphDimension::usage = "graphDimension[g, rmax, nSamples] estimates the ball-growth dimension d from |B_r| ~ r^d (log-log slope), averaged over random centres. Returns <|\"d\"->.., \"radii\"->.., \"counts\"->..|>.";
meshGraph::usage      = "meshGraph[mr] returns the (undirected, unweighted) vertex-adjacency graph of a triangle MeshRegion.";

Begin["`Private`"];

(* ---- Ollivier-Ricci via a transport linear program -------------------- *)
wasserstein1[nx_, ny_, ax_, ay_, dist_] := Module[{na, nb, cost, amat},
  na = Length[nx]; nb = Length[ny];
  cost = Flatten[Outer[dist[[#1, #2]] &, nx, ny]];
  amat = Join[
    Table[Boole[Quotient[k - 1, nb] + 1 == i], {i, na}, {k, na nb}],
    Table[Boole[Mod[k - 1, nb] + 1 == j], {j, nb}, {k, na nb}]];
  cost . LinearProgramming[cost, amat, Thread[{Join[ax, ay], 0}],
     Table[{0, Infinity}, {na nb}]]];

ollivierRicci[g_Graph, {x_, y_}, dist_] := Module[{nx, ny, w1},
  nx = AdjacencyList[g, x]; ny = AdjacencyList[g, y];
  If[nx === {} || ny === {}, Return[0.]];
  w1 = wasserstein1[nx, ny, ConstantArray[1./Length[nx], Length[nx]],
     ConstantArray[1./Length[ny], Length[ny]], dist];
  1. - w1/dist[[x, y]]];

ollivierRicci[g_Graph, {x_, y_}] :=
  ollivierRicci[g, {x, y}, GraphDistanceMatrix[g]];

ollivierRicci[g_Graph] := Module[{vl, idx, dist, ed},
  vl = VertexList[g]; idx = First /@ PositionIndex[vl];
  dist = GraphDistanceMatrix[g];
  ed = EdgeList[g];
  Association[(# -> ollivierRicci[g, {idx[#[[1]]], idx[#[[2]]]}, dist]) & /@ ed]];

(* ---- Forman-Ricci (triangle-augmented, unit weights) ------------------- *)
formanRicci[g_Graph] := Module[{ed, tri, triCount},
  ed = EdgeList[g];
  tri = FindCycle[g, {3}, All];   (* may be slow on large graphs *)
  triCount[u_, v_] := Count[tri,
    c_ /; SubsetQ[Union[Flatten[c /. UndirectedEdge -> List]], {u, v}]];
  Association[(# -> With[{u = #[[1]], v = #[[2]]},
       4 - VertexDegree[g, u] - VertexDegree[g, v] + 3 triCount[u, v]]) & /@ ed]];

(* ---- emergent dimension from ball growth ------------------------------- *)
graphDimension[g_Graph, rmax_Integer, nSamples_Integer] := Module[
   {dist, n, centers, counts, radii, win, logr, logn, slope},
   n = VertexCount[g];
   dist = GraphDistanceMatrix[g];
   centers = RandomSample[Range[n], Min[nSamples, n]];
   radii = Range[rmax];
   counts = Table[
     N@Mean[Table[Count[dist[[c]], x_ /; 0 <= x <= r], {c, centers}]], {r, radii}];
   (* fit the log-log slope over the UPPER radius window: small r is biased
      by the discrete +linear corrections to |B_r| ~ r^d *)
   win = Range[Max[2, Ceiling[rmax/2]], rmax];
   logr = Log[N@radii[[win]]]; logn = Log[counts[[win]]];
   slope = With[{m = Length[logr]},
     (m Total[logr logn] - Total[logr] Total[logn])/
      (m Total[logr^2] - Total[logr]^2)];
   <|"d" -> slope, "radii" -> radii, "counts" -> counts|>];

(* ---- mesh -> graph ----------------------------------------------------- *)
meshGraph[mr_MeshRegion] := Module[{tris, edges},
  tris = MeshCells[mr, 2][[All, 1]];
  edges = Union[Sort /@ Flatten[
      ({{#[[1]], #[[2]]}, {#[[2]], #[[3]]}, {#[[3]], #[[1]]}}) & /@ tris, 1]];
  Graph[Range[Length[MeshCoordinates[mr]]], UndirectedEdge @@@ edges]];

End[];
EndPackage[];
