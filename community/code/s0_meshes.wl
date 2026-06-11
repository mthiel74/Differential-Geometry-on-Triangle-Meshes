(* The two closed-form test surfaces used throughout. *)
R = 2.; r = 0.8;
torus  = DDG`torusMesh[R, r, 80, 160];   (* closed genus-1 torus from a periodic (u,v) grid *)
sphere = DDG`icosphereMesh[4];           (* icosahedron subdivided 4x, projected to the unit sphere *)
{Length[DDG`meshCoords[torus]], Length[DDG`meshCoords[sphere]]}   (* vertex counts *)
