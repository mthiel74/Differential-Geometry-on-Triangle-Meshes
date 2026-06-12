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
formanRicci::usage    = "formanRicci[g] returns <|edge -> curvature|> using the triangle-augmented Forman-Ricci formula F(u,v) = 4 - deg(u) - deg(v) + 3*(triangles on uv). Cheap and purely local.";
formanRicciMean::usage = "formanRicciMean[g] returns the mean Forman-Ricci curvature over all edges.";
ollivierRicciMean::usage = "ollivierRicciMean[g, n] estimates the mean Ollivier-Ricci curvature over a random sample of n edges (all edges if fewer).";
graphDimension::usage = "graphDimension[g, rmax, nSamples] estimates the ball-growth dimension d from |B_r| ~ r^d (log-log slope), averaged over random centres. Returns <|\"d\"->.., \"radii\"->.., \"counts\"->..|>.";
ricciCoarsen::usage   = "ricciCoarsen[g, frac] coarse-grains a graph by repeatedly contracting the edge of maximal Forman-Ricci curvature (the most redundant, clustered edge) until ~frac of the vertices remain. A fast, curvature-guided graph renormalization. ricciCoarsen[g, frac, labels] also returns the merged group each super-vertex came from.";
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

(* the core works in position space, so relabel vertices to 1..n first
   (IndexGraph); otherwise AdjacencyList[g, position] would use the position
   as a vertex name and silently return wrong neighbours. *)
ollivierRicci[g0_Graph] := Module[{g = IndexGraph[g0], dist, ed},
  dist = GraphDistanceMatrix[g];
  ed = EdgeList[g];
  Association[(# -> ollivierRicci[g, {#[[1]], #[[2]]}, dist]) & /@ ed]];

ollivierRicciMean[g0_Graph, nSamples_Integer: 200] := Module[
   {g = IndexGraph[g0], dist, es, sample},
   dist = GraphDistanceMatrix[g];
   es = EdgeList[g];
   sample = If[Length[es] <= nSamples, es, RandomSample[es, nSamples]];
   Mean[(ollivierRicci[g, {#[[1]], #[[2]]}, dist] &) /@ sample]];

(* ---- Forman-Ricci (triangle-augmented, unit weights) ------------------- *)
(* F(u,v) = 4 - deg(u) - deg(v) + 3*(# triangles on edge uv).  The triangle
   count is just (A^2)_{uv}; everything is degrees + one sparse matrix square,
   so this is far cheaper than the optimal-transport Ollivier-Ricci.          *)
formanRicci[g0_Graph] := Module[{g = IndexGraph[g0], a, a2, deg, ed},
  a = AdjacencyMatrix[g]; a2 = a . a; deg = Total[a]; ed = EdgeList[g];
  Association[(# -> (4 - deg[[#[[1]]]] - deg[[#[[2]]]] + 3 a2[[#[[1]], #[[2]]]])) & /@ ed]];

formanRicciMean[g_Graph] := Mean[N@Values[formanRicci[g]]];

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

(* ---- Forman-guided coarsening (a curvature renormalization) ------------ *)
(* Repeatedly contract the edge of maximal Forman-Ricci curvature -- the most
   clustered, redundant edge -- merging local structure while preserving the
   large-scale skeleton. Inline Forman keeps stable vertex ids so we can track
   which original vertices each super-vertex absorbed.                        *)
ricciCoarsen[g0_Graph, frac_] := ricciCoarsen[g0, frac, None];
ricciCoarsen[g0_Graph, frac_, labels_] := Module[
   {edges, members, n0, target, adj, deg, best, u, v, g, groups},
   edges = DeleteDuplicates[Sort /@ (List @@@ EdgeList[IndexGraph[g0]])];
   n0 = VertexCount[g0]; target = Max[1, Ceiling[frac n0]];
   members = AssociationThread[Range[n0] -> (List /@ Range[n0])];
   While[Length[members] > target && edges =!= {},
     adj = Merge[Rule @@@ Join[edges, Reverse /@ edges], Identity];
     deg = Length /@ adj;
     best = First@MaximalBy[edges,
        Function[ed, 4 - deg[ed[[1]]] - deg[ed[[2]]] +
           3 Length[Intersection[adj[ed[[1]]], adj[ed[[2]]]]]]];
     u = best[[1]]; v = best[[2]];
     members[u] = Join[members[u], members[v]]; KeyDropFrom[members, v];
     edges = DeleteCases[DeleteDuplicates[Sort /@ (edges /. v -> u)], {x_, x_}]];
   g = Graph[Keys[members], UndirectedEdge @@@ edges];
   If[labels === None, g,
     groups = (Commonest[labels[[#]]][[1]] &) /@ Values[members];
     {g, AssociationThread[Keys[members] -> groups], members}]];

(* ---- mesh -> graph ----------------------------------------------------- *)
meshGraph[mr_MeshRegion] := Module[{tris, edges},
  tris = MeshCells[mr, 2][[All, 1]];
  edges = Union[Sort /@ Flatten[
      ({{#[[1]], #[[2]]}, {#[[2]], #[[3]]}, {#[[3]], #[[1]]}}) & /@ tris, 1]];
  Graph[Range[Length[MeshCoordinates[mr]]], UndirectedEdge @@@ edges]];

End[];
EndPackage[];
