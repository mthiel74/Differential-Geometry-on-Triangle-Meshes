(* A Wolfram-model hypergraph after a few generations of one rewriting rule.
   One-off and needs internet (ResourceFunction); the spatial graph is cached
   to data/wm_graph.wxf so the rest of the notebook runs offline. *)
rule = {{1, 2, 3}} -> {{1, 2, 4}, {4, 3, 5}, {5, 2, 1}};
ev = ResourceFunction["WolframModel"][rule, {{1, 1, 1}}, 5];   (* 5 generations *)
ev["FinalStatePlot", ImageSize -> 460]
