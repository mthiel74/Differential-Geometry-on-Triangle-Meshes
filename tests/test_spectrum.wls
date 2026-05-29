#!/usr/bin/env wolframscript
(* test_spectrum.wls — the Laplace-Beltrami spectrum of the unit sphere must
   be lambda_l = l(l+1) with multiplicity 2l+1: {0, 2,2,2, 6,6,6,6,6, ...}.  *)

Needs["DDG`", FileNameJoin[{ParentDirectory[DirectoryName[$InputFileName]], "wolfram", "ddg.wl"}]];

sphere = DDG`icosphereMesh[3];
{vals, vecs} = DDG`laplacianSpectrum[sphere, 9];
exact = {0, 2, 2, 2, 6, 6, 6, 6, 6};         (* l = 0,1,2 blocks *)
maxRelErr = Max[MapThread[
   If[#2 == 0, Abs[#1], Abs[#1 - #2]/#2] &, {vals, exact}]];

(* M-orthonormality of the eigenvectors *)
m = DDG`massMatrix[sphere];
gram = vecs . m . Transpose[vecs];
orthoErr = Max[Abs[gram - IdentityMatrix[Length[vecs]]]];

pass = maxRelErr < 0.01 && orthoErr < 1.*^-8;
Print["max rel err vs l(l+1) (l<=2) = ", maxRelErr];
Print["M-orthonormality error        = ", orthoErr];
Print[If[pass, "PASS", "FAIL"]];
Exit[If[pass, 0, 1]];
