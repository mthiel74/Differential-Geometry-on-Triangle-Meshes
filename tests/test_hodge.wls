#!/usr/bin/env wolframscript
(* test_hodge.wls — discrete exterior calculus structural identities and the
   Hodge-Helmholtz decomposition.

   Checks: d1.d0 = 0 (d^2 = 0); d0^T star1 d0 = -L (DEC re-derives the cotan
   Laplacian); first Betti number b1 = 2 on a torus, 0 on a sphere; a 1-form
   reconstructs exactly from its exact+coexact+harmonic parts, which are
   mutually star1-orthogonal. *)

Needs["DDG`", FileNameJoin[{ParentDirectory[DirectoryName[$InputFileName]], "wolfram", "ddg.wl"}]];

tor = DDG`torusMesh[2., 0.8, 16, 32];
dec = DDG`decOperators[tor];
d0 = dec["d0"]; d1 = dec["d1"]; s1 = dec["star1"];

dd = Max[Abs[d1 . d0]];
lapErr = Max[Abs[Transpose[d0] . s1 . d0 + DDG`cotanLaplacian[tor]]];
b1tor = DDG`bettiOne[tor];
b1sph = DDG`bettiOne[DDG`icosphereMesh[2]];

SeedRandom[3];
omega = RandomReal[{-1, 1}, Length[dec["edges"]]];
h = DDG`hodgeDecomposition[tor, omega];
recon = Max[Abs[omega - (h["exact"] + h["coexact"] + h["harmonic"])]];
ip[a_, b_] := a . s1 . b;
ortho = Max[Abs[{ip[h["exact"], h["coexact"]], ip[h["exact"], h["harmonic"]],
    ip[h["coexact"], h["harmonic"]]}]]/ip[omega, omega];

pass = dd == 0. && lapErr < 1.*^-9 && b1tor == 2 && b1sph == 0 &&
   recon < 1.*^-9 && ortho < 1.*^-4;
Print["max|d1.d0| = ", dd, "   max|d0^T s1 d0 + L| = ", lapErr];
Print["b1 torus = ", b1tor, "   b1 sphere = ", b1sph];
Print["recon err = ", recon, "   normalised orthogonality = ", ortho];
Print[If[pass, "PASS", "FAIL"]];
Exit[If[pass, 0, 1]];
