#!/usr/bin/env python3
"""prepare_trex.py — one-off preprocessing of a high-resolution museum-cast
3D scan into clean, decimated triangle meshes usable by the pure-Wolfram DDG
pipeline.

The source is the photogrammetric scan "Tyrannosaurus rex skull (MOTE)" by
Emily Hauf for the Digital Atlas of Ancient Life (Paleontological Research
Institution / Museum of the Earth, Ithaca NY), released under CC0 (public
domain). See data/CREDITS.md. We weld duplicated vertices, keep the largest
connected component, decimate with quadric error metrics, orient consistently,
and normalise to a unit bounding sphere centred at the origin.

Outputs (committed to data/ so the WL pipeline never needs Python):
  data/trex.obj         ~12000 faces  (curvature, geodesics)
  data/trex_coarse.obj  ~2400 faces   (dense spectral solve)

Usage:  python3 scripts/prepare_trex.py [SRC_OBJ]
Requires: open3d, numpy.
"""
import sys, os
import numpy as np
import open3d as o3d

SRC = sys.argv[1] if len(sys.argv) > 1 else os.path.expanduser(
    "~/GitHub/Wolfram3DObjects/vertebrate-tyrannosaurus-rex-skull-mote/"
    "source/PRI_TyrannosaurusRexSkull/PRI_TyrannosaurusRexSkull.obj")
OUT_DIR = os.path.join(os.path.dirname(__file__), "..", "data")


def clean(m):
    m.remove_duplicated_triangles()
    m.remove_degenerate_triangles()
    m.remove_duplicated_vertices()
    m.remove_unreferenced_vertices()
    m.remove_non_manifold_edges()
    return m


def largest_component(m):
    labels, counts, _ = m.cluster_connected_triangles()
    labels = np.asarray(labels)
    keep = int(np.argmax(np.asarray(counts)))
    m.remove_triangles_by_mask(labels != keep)
    m.remove_unreferenced_vertices()
    return m


def normalise(m):
    v = np.asarray(m.vertices)
    v = v - v.mean(axis=0)
    v = v / np.linalg.norm(v, axis=1).max()        # unit bounding sphere
    m.vertices = o3d.utility.Vector3dVector(v)
    return m


def decimate(m, n_faces, tag):
    d = m.simplify_quadric_decimation(target_number_of_triangles=n_faces)
    d = clean(d)
    d = largest_component(d)
    d.orient_triangles()                            # consistent winding
    d = normalise(d)
    print(f"  {tag}: {len(d.vertices)} verts, {len(d.triangles)} faces, "
          f"edge_manifold={d.is_edge_manifold()}")
    return d


def main():
    print(f"reading {SRC}")
    m = o3d.io.read_triangle_mesh(SRC)
    print(f"  raw: {len(m.vertices)} verts, {len(m.triangles)} faces")
    m = m.merge_close_vertices(1e-6)
    m = clean(m)
    m = largest_component(m)
    print(f"  welded+cleaned: {len(m.vertices)} verts, {len(m.triangles)} faces")

    os.makedirs(OUT_DIR, exist_ok=True)
    for n_faces, name in [(12000, "trex.obj"), (2400, "trex_coarse.obj")]:
        d = decimate(m, n_faces, name)
        path = os.path.normpath(os.path.join(OUT_DIR, name))
        o3d.io.write_triangle_mesh(path, d, write_vertex_normals=False,
                                   write_vertex_colors=False)
        print(f"  wrote {path} ({os.path.getsize(path)} B)")


if __name__ == "__main__":
    main()
