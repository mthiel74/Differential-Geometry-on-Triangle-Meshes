# Differential Geometry on Triangle Meshes

A from-scratch, reproducible exploration of **discrete differential
geometry (DDG)** — how the smooth notions of curvature, normals, the
Laplace–Beltrami operator and curvature flow are defined on, and
computed from, **triangle meshes** — implemented in pure
**Wolfram Language**.

The end-product is a self-contained **Wolfram Community** notebook (see
`community/`), in the spirit of
[ENSO-emergence](https://github.com/mthiel74/ENSO-emergence) and
[Contiguous-Cartograms](https://github.com/mthiel74/Contiguous-Cartograms).

## What the project explores

1. **Discrete curvature** — per-vertex Gaussian curvature from the
   **angle defect** (the failure of the surrounding triangle angles to
   sum to 2π), and per-vertex mean curvature from the cotangent
   Laplacian of the position field. Validated against the
   **Gauss–Bonnet theorem**: the total angle defect of a closed mesh
   equals 2π·χ (χ the Euler characteristic).
2. **Discrete normals & areas** — area-weighted and angle-weighted
   vertex normals; the barycentric and Voronoi (mixed) vertex areas
   that the curvature operators are normalised by.
3. **The cotangent Laplace–Beltrami operator** — the workhorse of DDG;
   its spectrum, and its use in smoothing, parameterisation and flow.
4. **Curvature flow** — mean-curvature flow as a surface-fairing /
   denoising process; the difference between explicit and (stable)
   semi-implicit integration.
5. **Geodesics & distance** — the heat method for geodesic distance
   (Crane, Weischedel & Wardetzky 2013) as a showcase of the Laplacian.

Each notion is checked against a smooth reference where one exists
(sphere, torus) so the discrete answer is never asserted without a
convergence or conservation check.

## Repository layout

The whole pipeline is **pure Wolfram Language**.

| path                | what lives there                                                       |
| ---                 | ---                                                                    |
| `wolfram/*.wls`     | standalone exploration / figure-rendering scripts                      |
| `wolfram/*.wl`      | shared packages (mesh helpers, operators) loaded by the scripts        |
| `data/`             | small committed meshes / numeric snapshots for reproducibility         |
| `data/raw/`         | bulk raw mesh downloads (git-ignored, regenerable)                     |
| `community/`        | the buildable Wolfram Community notebook + its `.wls` source           |
| `docs/images/`      | rendered figures referenced from the notebook and this README          |
| `tests/`            | sanity checks (Gauss–Bonnet, operator symmetry, convergence)           |

## Reproducing

```sh
# §1 Angle-defect Gaussian curvature on a torus + Gauss–Bonnet check
wolframscript -file wolfram/curvature.wls

# §2 Cotangent Laplace–Beltrami operator + mean curvature (sphere H=1, torus H(u))
wolframscript -file wolfram/meancurvature.wls

# §3 Spectral geometry: LBO eigenfunctions, sphere spectrum l(l+1), smoothing
wolframscript -file wolfram/spectral.wls

# §4 Mean-curvature flow (semi-implicit fairing) + shrinking-sphere rate
wolframscript -file wolfram/meanflow.wls

# §5 Heat-method geodesic distance (sphere convergence + figures)
wolframscript -file wolfram/heat.wls

# §6 Discrete exterior calculus + Hodge decomposition on a torus
wolframscript -file wolfram/hodge.wls

# §7 Convergence / order-of-accuracy study (curvature O(h²), geodesics, Gauss–Bonnet)
wolframscript -file wolfram/convergence.wls

# §8 The operators on a real 3D scan (decimated Yoda figurine)
wolframscript -file wolfram/yoda.wls

# §9 Discrete curvature + emergent dimension on graphs (Ollivier–Ricci, ball growth)
wolframscript -file wolfram/graphcurvature.wls

# §10 A Wolfram-Physics hypergraph: emergent (hyperbolic) geometry
wolframscript -file wolfram/wolframmodel.wls

# Export live rotatable Graphics3D for the notebook (docs/models/*.wxf)
wolframscript -file wolfram/models.wls

# Sanity checks (all exit 0)
for t in tests/test_*.wls; do wolframscript -file "$t"; done

# Build the Wolfram Community notebook (community/ddg.nb + .pdf)
wolframscript -file community/build_notebook.wls
```

The notebook embeds the 3D figures as **live, rotatable `Graphics3D`**
objects (drag to rotate in the front end / on Wolfram Community), and adds
the deeper theory — shape operator and *Theorema Egregium*, Weyl's law and
Shape-DNA, the CFL stability bound, the cut locus, de Rham cohomology — plus
a measured **order-of-accuracy** study (§7): the pointwise operators are
second order (p≈2), the heat method sub-linear, and Gauss–Bonnet exact at
every resolution.

The project then goes **beyond meshes to graphs** (§9–10): **Ollivier–Ricci
curvature** via optimal transport (validated: complete graph +, cycle/grid 0,
tree −), **emergent dimension** from ball growth (recovers 1/2/3 on lattices
and ≈2 on the mesh-graphs), the **soccer-ball theorem** (a sphere's mesh-graph
carries positive curvature at exactly 12 degree-5 defects — a combinatorial
Gauss–Bonnet), and a bridge to the **Wolfram Physics Project**: the same tools
show a rewriting hypergraph grows a *hyperbolic* (negatively curved,
super-polynomial) emergent space. `wolfram/graphgeometry.wl` holds the graph
operators.

Figures land in `docs/images/`. The narrative arc — the
"one matrix (the cotangent Laplacian) does everything" thread —
is **curvature → Laplace–Beltrami → spectrum → curvature flow →
heat-method geodesics → DEC / Hodge decomposition**, all implemented and
each validated against a smooth reference or a topological invariant.
The same operators are then run on a **3D-scanned Yoda** figurine
(decimated via `scripts/prepare_yoda.py`) to show they work on real,
messy geometry.

### A note on test meshes

`DiscretizeRegion[Sphere[…]]` produces an irregular triangulation with
**inconsistently-oriented** triangles, which wrecks vertex normals and
biases the cotangent mean-curvature estimate. The operators are therefore
validated on a **regular geodesic icosphere** (`DDG`icosphereMesh`, an
icosahedron subdivided and projected to the sphere) and a structured
**torus grid** (`DDG`torusMesh`), both of which are consistently wound.
On these, discrete mean curvature matches the smooth value to ~10⁻³ (sphere)
and ~3×10⁻⁴ (torus).

## Status

Active — early exploration. Target end-product: a Wolfram Community
post once the DDG notions and figures are in place.

## Related projects

* [ENSO-emergence](https://github.com/mthiel74/ENSO-emergence)
* [Contiguous-Cartograms](https://github.com/mthiel74/Contiguous-Cartograms)
