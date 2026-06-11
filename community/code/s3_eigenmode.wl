(* A single Laplace-Beltrami eigenfunction on the sphere (a discrete spherical
   harmonic), drawn live. laplacianSpectrum solves (-L) phi = lambda M phi. *)
sph = DDG`icosphereMesh[4];
sp = DDG`meshCoords[sph]; st = DDG`meshTriangles[sph];
{vals, vecs} = DDG`laplacianSpectrum[sph, 6];     (* 6 smallest eigenpairs *)

Graphics3D[{EdgeForm[],
    GraphicsComplex[sp, Polygon[st],
      VertexColors -> Map[ColorData["BlueGreenYellow"], Rescale[vecs[[5]]]]]},
   Boxed -> False, SphericalRegion -> True, Lighting -> "Neutral", ImageSize -> 360]
