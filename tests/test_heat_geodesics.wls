#!/usr/bin/env wolframscript
(* test_heat_geodesics.wls — the heat method recovers geodesic distance on the
   unit sphere: distance from the north pole = colatitude arccos(z).         *)

Needs["DDG`", FileNameJoin[{ParentDirectory[DirectoryName[$InputFileName]], "wolfram", "ddg.wl"}]];

sph = DDG`icosphereMesh[4];
pts = DDG`meshCoords[sph];
src = First@Ordering[pts[[All, 3]], -1];
phi = DDG`heatGeodesics[sph, src];
exact = ArcCos[Clip[pts[[All, 3]], {-1., 1.}]];
relErr = Norm[phi - exact]/Norm[exact];

pass = relErr < 0.02 && Min[phi] >= -1.*^-6;     (* distances non-negative *)
Print["rel L2 error vs arccos(z) = ", relErr, "   min phi = ", Min[phi]];
Print[If[pass, "PASS", "FAIL"]];
Exit[If[pass, 0, 1]];
