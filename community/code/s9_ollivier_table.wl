(* Ollivier-Ricci curvature (optimal transport, one linear program per edge)
   on canonical graphs: the signs come out as geometry predicts. *)
examples = {
   {"complete K6", CompleteGraph[6]},
   {"cycle C8", CycleGraph[8]},
   {"6x6 grid", GridGraph[{6, 6}]},
   {"binary tree", CompleteKaryTree[3, 3]}};

Grid[Prepend[
   {#[[1]], NumberForm[Mean[Values[GraphDDG`ollivierRicci[#[[2]]]]], {4, 3}]} & /@ examples,
   {"graph", "mean Ollivier-Ricci"}], Frame -> All]
