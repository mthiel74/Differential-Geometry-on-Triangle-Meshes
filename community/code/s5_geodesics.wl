(* Heat-method geodesic distance (Crane-Weischedel-Wardetzky 2013): three
   linear solves in L and M. Banded colouring shows the wavefronts. *)
banded[v_, nb_] := (ColorData["Rainbow"][Round[# nb]/nb] &) /@ (v/Max[v]);

sph = DDG`icosphereMesh[4]; sp = DDG`meshCoords[sph]; st = DDG`meshTriangles[sph];
gsrc = First@Ordering[sp[[All, 3]], -1];            (* north pole *)
sphereGeo = DDG`heatGeodesics[sph, gsrc];

tg = DDG`torusMesh[2., 0.8, 40, 80];
tgp = DDG`meshCoords[tg]; tgt = DDG`meshTriangles[tg];
tgsrc = First@Ordering[tgp[[All, 1]], -1];          (* a point on the outer equator *)
torusGeo = DDG`heatGeodesics[tg, tgsrc];

surf[pts_, tris_, phi_, nb_] := Graphics3D[{EdgeForm[],
     GraphicsComplex[pts, Polygon[tris], VertexColors -> banded[phi, nb]]},
   Boxed -> False, Lighting -> "Neutral", SphericalRegion -> True, ImageSize -> 300];

Row[{surf[sp, st, sphereGeo, 16], surf[tgp, tgt, torusGeo, 18]}]
