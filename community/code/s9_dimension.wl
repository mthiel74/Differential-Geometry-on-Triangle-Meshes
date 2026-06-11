(* Emergent dimension from geodesic-ball growth |B_r| ~ r^d, on periodic
   lattices (d = 1, 2, 3) and on our meshes viewed as bare graphs (d = 2). *)
ring = CycleGraph[200];
tor2 = GraphProduct[CycleGraph[40], CycleGraph[40], "Cartesian"];
tor3 = GraphProduct[GraphProduct[CycleGraph[14], CycleGraph[14], "Cartesian"],
   CycleGraph[14], "Cartesian"];
sphG = GraphDDG`meshGraph[DDG`icosphereMesh[3]];
torG = GraphDDG`meshGraph[DDG`torusMesh[2., 0.8, 24, 48]];

dimRuns = {{"ring C(200)", ring, 12}, {"2-torus", tor2, 12}, {"3-torus", tor3, 7},
   {"sphere mesh-graph", sphG, 7}, {"torus mesh-graph", torG, 8}};
dimData = Table[{run[[1]], GraphDDG`graphDimension[run[[2]], run[[3]], 18]}, {run, dimRuns}];

ListLogLogPlot[Table[Transpose[{d[[2]]["radii"], d[[2]]["counts"]}], {d, dimData}],
   Joined -> True, PlotMarkers -> Automatic, Frame -> True,
   FrameLabel -> {"radius r", "ball size |B_r|"},
   PlotLegends -> Placed[
     (#[[1]] <> "  (d=" <> ToString[NumberForm[#[[2]]["d"], {3, 2}]] <> ")" & /@ dimData), Right],
   ImageSize -> 640, GridLines -> Automatic]
