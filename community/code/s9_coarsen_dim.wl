(* Forman-guided coarsening (a curvature renormalization): repeatedly contract
   the edge of maximal Forman curvature. A 2D mesh-graph keeps d ~ 2 as it shrinks. *)
tg = GraphDDG`meshGraph[DDG`torusMesh[2., 0.8, 26, 40]];
fracs = {1., 0.8, 0.65, 0.5, 0.4};
dimSeq = Table[
   Module[{c = If[f == 1., tg, GraphDDG`ricciCoarsen[tg, f]]},
    {VertexCount[c], GraphDDG`graphDimension[c, Min[7, GraphDiameter[c] - 1], 15]["d"]}],
   {f, fracs}];

ListLinePlot[dimSeq, PlotMarkers -> Automatic, Frame -> True,
   FrameLabel -> {"vertices after coarsening", "emergent dimension d"},
   PlotLabel -> Style["Dimension is ~scale-invariant under Forman coarsening", 12],
   PlotRange -> {0, 3}, GridLines -> {None, {2}},
   GridLinesStyle -> Directive[Green, Dashed], ImageSize -> 440]
