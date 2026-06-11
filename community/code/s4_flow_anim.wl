(* The flow as an animation: render every other (volume-rescaled) frame,
   coloured by mean curvature on a fixed scale so it reads as H equalising. *)
framesN = rescale /@ fr;
hrange = MinMax[Sign[Mean[#]] # &@ DDG`meanCurvature[DDG`triMesh[framesN[[1]], st]]];

renderFrame[x_] := Module[{H = DDG`meanCurvature[DDG`triMesh[x, st]]},
  H = Sign[Mean[H]] H;
  Graphics3D[{EdgeForm[], GraphicsComplex[x, Polygon[st],
      VertexColors -> (ColorData["TemperatureMap"][Rescale[#, hrange]] & /@ H)]},
    Boxed -> False, SphericalRegion -> True, ViewPoint -> {1.6, -2.4, 1.4},
    ImageSize -> 360, Lighting -> "Neutral",
    PlotRange -> 1.4 {{-1, 1}, {-1, 1}, {-1, 1}}]];

ListAnimate[renderFrame /@ framesN[[1 ;; -1 ;; 2]], AnimationRunning -> False]
