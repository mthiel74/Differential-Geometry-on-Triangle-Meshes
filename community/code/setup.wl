(* Initialization cell: loads the two accompanying packages automatically when
   the notebook is evaluated. Keep ddg.wl and graphgeometry.wl in the SAME folder
   as this notebook -- they are imported from NotebookDirectory[]. Every figure
   below is then preceded by the exact calls that produced it. *)
Needs["DDG`",      FileNameJoin[{NotebookDirectory[], "ddg.wl"}]];
Needs["GraphDDG`", FileNameJoin[{NotebookDirectory[], "graphgeometry.wl"}]];
