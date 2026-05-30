#!/usr/bin/env wolframscript
(* run_all.wls — regenerate every figure, then build the community notebook.
   One entry point for a full reproducible rebuild. *)

dir = DirectoryName[$InputFileName];
root = ParentDirectory[dir];

scripts = {
   FileNameJoin[{dir, "curvature.wls"}],
   FileNameJoin[{dir, "meancurvature.wls"}],
   FileNameJoin[{dir, "spectral.wls"}],
   FileNameJoin[{dir, "meanflow.wls"}],
   FileNameJoin[{dir, "heat.wls"}],
   FileNameJoin[{dir, "hodge.wls"}],
   FileNameJoin[{dir, "convergence.wls"}],
   FileNameJoin[{dir, "graphcurvature.wls"}],
   FileNameJoin[{dir, "yoda.wls"}],
   FileNameJoin[{root, "community", "build_notebook.wls"}]};

Do[
  Print["\n===== ", FileNameTake[s], " ====="];
  Run["wolframscript -file " <> "\"" <> s <> "\""],
  {s, scripts}];

Print["\nrun_all complete."];
