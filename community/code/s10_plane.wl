(* The curvature-dimension plane: kappa = 0 is the robust classifier. *)
ListPlot[
   MapThread[Labeled[#1, Style[#2, 9]] &,
     {{#["d"], #["k"]} & /@ results, #["label"] & /@ results}],
   Frame -> True, FrameLabel -> {"emergent dimension  d", "mean Ollivier-Ricci  \[Kappa]"},
   PlotLabel -> Style["The curvature-dimension plane: a geometric classification", 13],
   PlotStyle -> Directive[PointSize[0.025], RGBColor[0.8, 0.25, 0.1]],
   GridLines -> {None, {0}}, GridLinesStyle -> Directive[Gray, Dashed],
   PlotRange -> All, ImageSize -> 620, ImagePadding -> {{60, 90}, {50, 40}}]
