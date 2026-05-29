# Repo notes for Claude

## Purpose

Explore **discrete differential geometry on triangle meshes** in pure
Wolfram Language, building toward a **Wolfram Community** post
(`community/*.nb`) in the same style as the ENSO-emergence and
Contiguous-Cartograms repos.

## Pipeline (pure Wolfram Language — no Python)

```
wolfram/ddg.wl          shared package: mesh accessors + DDG operators
wolfram/curvature.wls   angle-defect Gaussian curvature + Gauss–Bonnet check
                        (more scripts added as the exploration grows)
community/build_notebook.wls   assembles the community notebook (later)
```

## Conventions

* Plain-text `.wls` / `.wl` is the source of truth. The `.nb` and
  `.pdf` in `community/` are committed *outputs*.
* Figures live in `docs/images/` only — referenced from both the
  README and the notebook.
* Every discrete quantity is validated against a smooth reference or a
  conservation law (Gauss–Bonnet, operator symmetry, convergence under
  refinement) before it is reported. Don't assert a number without the
  check that produced it.
* Use Wolfram's native mesh objects (`MeshRegion`, `MeshCoordinates`,
  `MeshCells`) and build meshes with `DiscretizeRegion` /
  `BoundaryDiscretizeRegion` so results are reproducible.

## Commit cadence

Commit + push after each meaningful step (skeleton, package, each
operator, each figure, notebook). Keep messages short and factual.
Repo is **private** for now.
