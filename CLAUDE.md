# Repo notes for Claude

## Purpose

Explore **discrete differential geometry on triangle meshes** in pure
Wolfram Language, building toward a **Wolfram Community** post
(`community/*.nb`) in the same style as the ENSO-emergence and
Contiguous-Cartograms repos.

## Pipeline (pure Wolfram Language — no Python)

```
wolfram/ddg.wl          shared package: mesh accessors, test meshes
                        (torusMesh, icosphereMesh) + DDG operators
                        (cotanLaplacian, massMatrix, curvatures,
                        implicitFairing, laplacianSpectrum)
wolfram/curvature.wls   §1 angle-defect Gaussian curvature + Gauss–Bonnet
wolfram/meancurvature.wls §1–2 cotan Laplacian + mean curvature
wolfram/spectral.wls    §3 LBO eigenfunctions, sphere spectrum, smoothing
wolfram/meanflow.wls    §4 mean-curvature flow (semi-implicit fairing)
wolfram/run_all.wls     regenerate all figures, then build the notebook
community/build_notebook.wls   assembles community/ddg.nb (+ .pdf)
tests/test_*.wls        one sanity check per operator (all exit 0)
```

## Narrative arc (the "one matrix" thread)

The post follows the cotangent Laplacian through: curvature →
Laplace–Beltrami → spectrum → mean-curvature flow → heat-method
geodesics → DEC/Hodge. §1–4 are implemented; the heat method
(Crane–Weischedel–Wardetzky 2013) and DEC/Hodge are next. This arc was
cross-checked with Codex and Gemini (both converged on it independently).

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
