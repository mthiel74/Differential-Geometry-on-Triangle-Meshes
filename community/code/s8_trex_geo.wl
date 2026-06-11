(* The operators on a real 3D scan: a 12k-triangle decimation of a
   photogrammetric Tyrannosaurus rex skull scan (a museum cast; CC0).
   Heat-method geodesic distance from the tip of the snout. *)
trex = Import["data/trex.obj", "MeshRegion"];
tp = DDG`meshCoords[trex]; tt = DDG`meshTriangles[trex];
tview = {ViewPoint -> {0.55, -2.25, 0.05}, ViewVertical -> {1, 0, 0}};  (* lateral *)

(* snout = the narrower end of the long (z) axis *)
zc = tp[[All, 3]]; {zlo, zhi} = MinMax[zc]; zlen = zhi - zlo;
spread[e_] := Module[{xy = Select[tp, Abs[#[[3]] - e] < 0.12 zlen &][[All, {1, 2}]]},
   RootMeanSquare[Flatten[(# - Mean[xy]) & /@ xy]]];
src = First@Ordering[Abs[zc - If[spread[zlo] < spread[zhi], zlo, zhi]], 1];

tGeo = DDG`heatGeodesics[trex, src];
banded[v_, nb_] := (ColorData["Rainbow"][Round[# nb]/nb] &) /@ (v/Max[v]);

Graphics3D[{EdgeForm[], GraphicsComplex[tp, Polygon[tt], VertexColors -> banded[tGeo, 22]]},
   Boxed -> False, Lighting -> "Neutral", ImageSize -> 420, Sequence @@ tview]
