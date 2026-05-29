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
# Angle-defect Gaussian curvature on a torus + Gauss–Bonnet check
wolframscript -file wolfram/curvature.wls

# Cotangent Laplace–Beltrami operator + mean curvature (sphere H=1, torus H(u))
wolframscript -file wolfram/meancurvature.wls

# Sanity checks
wolframscript -file tests/test_gauss_bonnet.wls
wolframscript -file tests/test_mean_curvature.wls
```

Both figures land in `docs/images/`. More entry points will be added as
the exploration grows.

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
