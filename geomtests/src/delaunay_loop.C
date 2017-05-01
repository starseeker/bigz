// Copyright 2004, 2006 Sylvain Pion, INRIA Sophia-Antipolis, France.
// Distributed under the Boost Software License, Version 1.0. (See accompany-
// ing file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//
// This file is part of 'Classroom Examples of Robustness Problems in Geometric
// Computations', <http://www.mpi-sb.mpg.de/~kettner/proj/NonRobust/>.
//
// delaunay_loop.C
// This program illustrates some robustness issues that happen in
// 3D Delaunay triangulation computation when using doubles. Based on CGAL.

// We build a Delaunay triangulation with 5 points, all on the convex hull.
// There is an internal edge incident to 3 tetrahedra.
// We then try to locate a point close to this edge, and show that it can
// easily loop.
//
// Note that the locate() is randomized, so it can happen that a data set
// loops on one run, and doesn't on the other.  We just iterate several point
// locations on the same triangulation, and it basically always finishes by
// looping on one location.  It is also possible to de-randomize locate() by
// hacking CGAL code (look for "remembering visibility walk" in
// include/CGAL/Triangulation_3.h).
//
// If geomview is available, the configuration can be visualized at the end
// by defining the GEOMVIEW_VIS macro.

#include <CGAL/Simple_cartesian.h>
#include <CGAL/Delaunay_triangulation_3.h>
#include <CGAL/IO/Geomview_stream.h>
#include <CGAL/IO/Triangulation_geomview_ostream_3.h>

// uncomment this line to use Geomview for visualization instead of looping
//#define GEOMVIEW_VIS

typedef CGAL::Simple_cartesian<double>           Kernel;
typedef CGAL::Delaunay_triangulation_3<Kernel>   Delaunay;
typedef Kernel::Point_3                          Point;
typedef Kernel::Segment_3                        Segment;
typedef Delaunay::Edge                           Edge;


// Random point with drand48() coordinates.
Point random_point() {
    return Point(drand48(), drand48(), drand48());
}

// Random point approximately on a segment.
Point random_point_on_segment(const Segment &s) {
    return s.source() + drand48() * (s.target() - s.source());
}

// Build a Delaunay triangulation with 5 points, all on the convex hull,
// such that we have an internal edge incident to 3 tetrahedra.
// Returns the internal edge.
Edge build_triangulation(Delaunay &d) {
    while (true) {
        // insert 5 random points into a Delaunay triangulation.
        d.clear();
        for ( int i = 0; i < 5; ++i)
            d.insert( random_point());

        // Now check if this is the configuration we want, loop otherwise.
        if (d.number_of_vertices() != 5 ||
            d.number_of_cells() != 9 ||
            d.number_of_finite_cells() != 3 )
            continue;

        // Try to pick an edge which has 3 finite cells as neighbors,
        // and 0 infinite cells.
        Delaunay::Finite_edges_iterator feit = d.finite_edges_begin();
        for ( ; feit != d.finite_edges_end(); ++feit) {
            Delaunay::Cell_circulator ccir = d.incident_cells(*feit);
            Delaunay::Cell_circulator begin = ccir;
            int nb_finite_cells = 0;
            bool on_boundary = false;
            do {
                if (d.is_infinite(ccir)) {
                    on_boundary = true;
                    break;
                }
                ++nb_finite_cells;
                ++ccir;
            } while (ccir != begin);
            if (on_boundary || nb_finite_cells != 3)
                continue;
            // Found desired internal edge.
            return *feit;
        }
    }
}

// Attempt to locate points close to the internal edge.
void try_to_locate(const Delaunay &d, const Edge &e) {
    for (int i=0; i<1000; ++i) {
        Point p = random_point_on_segment( d.segment(e));
        std::cout << "#" << i << " : " << p << std::endl;
        d.locate(p);
    }
    std::cout << " 1000 point locations did not loop, giving up." << std::endl;
}

int main(int argc, char **argv) {
    std::cout << "Usage : " << argv[0] << " [seed]" << std::endl;
    std::cout.precision(20);

    // Random generator seed.
    int seed = argc > 1 ? atoi(argv[1]) : getpid();
    srand48(seed);
    std::cout << "seed = " << seed << std::endl;
    
    // Computing the triangulation.
    Delaunay d;
    Edge e = build_triangulation(d);
    
    std::cout << " Using the Delaunay triangulation of the following points :"
              << std::endl;
    std::copy( d.points_begin(), d.points_end(),
               std::ostream_iterator<Point>(std::cout, "\n"));

#ifndef GEOMVIEW_VIS
    std::cout << " Now trying to locate the following points :" << std::endl;
    try_to_locate(d, e); // comment this line to use the Geomview visualization

#else // GEOMVIEW_VIS
    // To visualize the triangulation with Geomview.
    CGAL::Geomview_stream gv;
    gv.clear();
    gv << d;
    sleep(60);

#endif // GEOMVIEW_VIS
    
    return 0;
}
