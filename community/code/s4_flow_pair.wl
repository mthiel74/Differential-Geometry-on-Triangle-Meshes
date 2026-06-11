(* Mean-curvature flow by semi-implicit fairing: (M - tau L) x' = M x.
   A bumpy sphere is flowed 20 steps, then each frame is rescaled to constant
   enclosed volume so the eye sees smoothing rather than shrinking. *)
sph = DDG`icosphereMesh[4]; sp = DDG`meshCoords[sph]; st = DDG`meshTriangles[sph];
SeedRandom[7];
bumpy = ((1. + 0.16 #[[3]]^2 + 0.12 Sin[2 #[[1]]] Cos[2 #[[2]]]) Normalize[#]) & /@ sp;
fr = DDG`implicitFairing[bumpy, st, 6.*^-3, 20];   (* tau = 6e-3, 20 unconditionally-stable steps *)

vol[x_] := Abs[Total[(x[[#[[1]]]] . Cross[x[[#[[2]]]], x[[#[[3]]]]]) & /@ st]]/6.;
v0 = vol[fr[[1]]];
rescale[x_] := x (v0/vol[x])^(1./3.);
diverging[v_] := With[{m = Max[Abs[v]] + 1.*^-12},
   Map[ColorData["TemperatureMap"], Rescale[v, {-m, m}]]];
surf[x_, lbl_] := Graphics3D[{EdgeForm[], GraphicsComplex[x, Polygon[st],
     VertexColors -> diverging[Sign[Mean[#]] # &@ DDG`meanCurvature[DDG`triMesh[x, st]]]]},
   Boxed -> False, Lighting -> "Neutral", SphericalRegion -> True, ImageSize -> 290,
   PlotLabel -> lbl];

Row[{surf[rescale[fr[[1]]], "before flow"], surf[rescale[fr[[-1]]], "after mean-curvature flow"]}]
