# ssys map generic processing tools

## I/O
All you need to convert from/to a graph.
Input formats:
 - ssysmap: denotes current system map, that is stored in `ssys/*.xml`.
 - graph: (see `ssys2graph.sh -h` for format description)
 - dot: `graphviz` graph format. Can be used to generate positions and `png` output.

Output formats:
 - ssysmap
 - graph
 - dot
 - pov: povray file that allows to generate `png` with `povray`

Scripts:
 - `ssysmap2graph.sh`: Reads current system map, outputs a graph, see `ssys2graph.sh -h`.
 - `graph2ssysmap.py`: Reads a graph in input, updates current system map accordingly. This is the only way to actually modify it.

 - `dot2graph.py`: As the name suggests, turns a graph in dot format into a graph in our basic format.
 - `graph2pov.py`: Reads graph input, and generates a `png` using `povray`. Will have colors if input vertices have color tags (i.e. input is obtained from `ssys2graph | graph_vaux -c`.
 - `graph2dot.py`: As the name suggests, reads current system map and outputs a graph in dot format.

## graphmod
`graphmod.py` provides all that is necessary to build a graph modifier such as the programs described in the section graphmods below. As a simple example of use, see `graphmod_repos_virt.py` source. By convention, we call `graph_xxx.py` the generic utilities and `graphmod_xxx.py` the ad hoc modifiers.
 - `graph_vaux.py`: Reads a graph in input, adds faction tag to vertex aux field, and outputs the result. With, `-c`, adds the color instead of the faction. Also provides color values when imported.
 - `graph_scale.py`: Reads a graph in input, scales it, and outputs the resulting graph.


## auto-positioning system
Another graph modifier.

 - `reposition` is a graph modifier (graph in, graph out). If you want to reposition only a select set of systems, provide their names as arguments. See `reposition -h`.
    - `all_ssys_but.sh` allows to get the list of all systems except those provided in input.
 - `repos.sh` uses it. It reads ssys and applies reposition repeatedly. See `repos.sh -h`.

## potential
 - `potential -g` generates the __gravity potential__ of the star map as a picture (black means minimum potential, and white means 0 potential (upper bound).
 - `height_map.pov` allows to display that potential in 3D. Use `gen_pot.sh` to do it all.
 - `potential -a` can __apply potential__, see `apply_pot.sh`.


# Specific scripts and Current process

These are designed to change the star map by **changing systems position**.

## graphmods
 - `graphmod_prep.py`: A set virtual edge addition designed to serve as a pre-processing for `neato`.
 - `graphmod_postp.py`: A set of ad hoc operations designed to serve as a post-processing for neato output. See section below.
 - `graphmod_virtual_ssys.py`: gives a position of virtual systems (eg. gauntlet, test of ...)
 - `graphmod_smooth_tl.py`: smoothens the tradelane. Interesting in combination with `reposition`.
 - `graphmod_abh.py`: Repairs the stretching of Anubis Black Hole after gravity application.

## main process
Performed by `process_ssys.sh`. Several steps:
 - generate `map_bef.png`
 - __1__ Call `graphmod_prep` At this point, the __pre-processing__ occurs: some invisible edges are added to enforce desired properties. Then `graph2dot` is called to turn the graph into a dot file.
 - __2__ Call `neato` (from `graphviz` package) to compute the dot graph layout.
 - __3__ Call `dot2graph.py` to extract layout information from the dot input and output the resulting graph.
 - generate `map_dot.png`
 - __4__ Call `graphmod_pp.py`. At this point, the __post-processing__ occurs: some geometrical transformations are applied: compute the wild space layout, enforce co-circularity of some points around Anubis BH, rotate some parts, etc.
 - generate `map_post.png`
 - __5__ apply 3 times:
    - `reposition`
    - `graphmod_smooth_tl.py`
 - generate `map_repos.png`
 - __6__ `apply_pot.sh -g` applies gravity + some graphmods.
 - generate `map_grav.png`
 - __7__ A bit of post processing. See `process_ssys.sh` for more details.
 - generate `map_aft.png`

The labeled maps `before.png` and `after.png` are generated, with obvious meaning.

# Others

## ssys process tools
These are designed to manage the **internal geometry of systems**, that might get affected by the position changes. (because autojumps move when the systems move)

 - `ssys_freeze.py`: fixes the auto jumps position and marks them as previously auto.
 - `ssys_relax.py`: rotates a frozen system for minimizing orientation stretch. Uses `minimize_angle_stretch.py`.
 - `ssys_unfreeze.sh`: as the name suggests.
 - `ssys_empty.py`: gives the list of systems that can be considered empty.

## ssys lib
 - `geometry.py` as the name suggests. Most is really classical:
    - vec for (2d-)vectors. Support for every usual composition with float. `+` and `-` are what you expect, `*` is what you expect for floats and vectors (dot product). `/` is what you expect for floats. See the source for more functionalities.
    - transformations. Obtained by __dividing__ vectors: `v1/v2` is the transformation that turns `v2` into `v1`. As expected: `k * v2 * (v1/v2) = k*v1`. So multiplying vectors with transf. gives the result of the transformation applied to the vector. Notably, when `v1` and `v2` are normalized (or just have the same size), `v1/v2` is the rotation that turns `v2` into `v1`.
    - lines (line intersection) / circle (circumscribed / inscribed / bounding circle).
    - bb for bounding box. use += to enlarge the bb with a new element, and `in` to test if inside. Nothing fancy.
    - Bounding circle / bounded circle.
 - `ssys.py` everything else.

## ssysmap history
 - `ssysmap_{push, pop, dequeue}.sh` apply the corresponding operations to ssys map position history.

# TODO
 - `reposition.c`:
    - penalty map +
    - fixed point algorithm
    - possible opt: manage separately neigh with non-1.0 len.
    - fix tunnel effect -
 - maybe manage tags in `ssysmap2graph.sh` instead of `graph_vaux.py`
