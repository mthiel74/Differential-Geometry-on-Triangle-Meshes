(* Gaussian curvature K (angle defect) and mean curvature H (cotan Laplacian)
   on the torus, drawn as two live, rotatable panels. *)
R = 2.; r = 0.8;
tor = DDG`torusMesh[R, r, 48, 96];
tp = DDG`meshCoords[tor]; tt = DDG`meshTriangles[tor];

K = DDG`gaussianCurvature[tor];                 (* = angleDefect / vertexArea *)
H = DDG`meanCurvature[tor]; H = Sign[Mean[H]] H; (* fix the global sign *)

diverging[v_] := With[{m = Max[Abs[v]] + 1.*^-12},
   Map[ColorData["TemperatureMap"], Rescale[v, {-m, m}]]];
surf[cols_] := Graphics3D[{EdgeForm[],
     GraphicsComplex[tp, Polygon[tt], VertexColors -> cols]},
   Boxed -> False, Lighting -> "Neutral", SphericalRegion -> True, ImageSize -> 300];

Row[{surf[diverging[K]], surf[Map[ColorData["TemperatureMap"], Rescale[H]]]}]
