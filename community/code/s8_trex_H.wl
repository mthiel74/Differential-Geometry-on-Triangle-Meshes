(* Mean curvature on the scan, robustly clipped to suppress scan noise. *)
tH = DDG`meanCurvature[trex]; tH = Sign[Median[tH]] tH;
{tlo, thi} = Quantile[tH, {0.03, 0.97}];           (* robust clip *)

Graphics3D[{EdgeForm[], GraphicsComplex[tp, Polygon[tt],
     VertexColors -> Map[ColorData["ThermometerColors"],
       Rescale[Clip[tH, {tlo, thi}], {tlo, thi}]]]},
   Boxed -> False, Lighting -> "Neutral", ImageSize -> 420, Sequence @@ tview]
