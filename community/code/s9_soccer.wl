(* The "soccer-ball theorem": a hexagonal sphere is forced to carry exactly
   twelve degree-5 vertices, and that is where the positive curvature sits. *)
ico = DDG`icosphereMesh[3];
ipp = DDG`meshCoords[ico]; itt = DDG`meshTriangles[ico];
deg5 = Flatten@Position[VertexDegree[GraphDDG`meshGraph[ico]], 5];   (* the 12 defects *)

Graphics3D[{
    {EdgeForm[GrayLevel[0.45]], FaceForm[GrayLevel[0.92]], GraphicsComplex[ipp, Polygon[itt]]},
    {RGBColor[0.85, 0.15, 0.15], Sphere[ipp[[#]], 0.05] & /@ deg5}},
   Boxed -> False, Lighting -> "Neutral", SphericalRegion -> True, ImageSize -> 400]
