// Copyright 2003, 2004, 2006 Lutz Kettner, Max-Planck-Institute 
// fuer Informatik, Saarbruecken (Germany).
// Distributed under the Boost Software License, Version 1.0. (See accompany-
// ing file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//
// This file is part of 'Classroom Examples of Robustness Problems in Geometric
// Computations', <http://www.mpi-sb.mpg.de/~kettner/proj/NonRobust/>.
//
// incr_convex_hull.C
// A simple incremental algorithm for 2d convex hull.
// Main program for testing and demo. No visualization.

#include <list>
#include <algorithm>
#include <fstream>
#include <string.h>

#include <points.h>
#include <incr_convex_hull.h>

int main( int argc, char** argv) {
    if ( argc > 1 && ( strcmp( argv[1], "-h") == 0 
                    || strcmp( argv[1], "--help") == 0)) {
        std::cerr << 
"Usage: incr_convex_hull [<Options>] [<files> ...]\n"
"    2D convex hull of the points in <files>. If none is given and \n"
"    and option -r is not used, reads points from stdin.\n"
"    Writes convex hull points in counterclockwise order to stdout.\n" 
"Options:\n"
"    -h/--help:      this message\n"
"    -q:             quite. No output of the convex hull points\n"
"    -v1:            verbose level 1: algorithm control flow\n"
"    -r/--rand <n>:  creates <n> points randomly chosen from the unit disc\n";
        return 0;
    }
    std::list<Point> points;
    bool read_from_stdin = true;
    bool quiet           = false;
    for ( int i = 1; i < argc; ++i) {
        if ( strcmp( argv[i], "-r") == 0 || strcmp( argv[i], "--rand") == 0) {
            read_from_stdin = false;
            ++i;
            int n = 100;
            if ( i < argc)
                n = atoi( argv[i]);
            if ( n < 0 || n > 1000000) {
                std::cerr << "Error: choose parameter <n> for -r/--rand "
                             "between [0,1000000]." << std::endl;
                return 1;
            }
            random_fill( n, std::back_inserter(points));
        } else if ( strcmp( argv[i], "-q") == 0){
            quiet = true;
        } else if ( strcmp( argv[i], "-v1") == 0){
            verbose_algo = true;
        } else if ( strcmp( argv[i], "-v2") == 0){
            verbose_pred = true;
        } else {
            read_from_stdin = false;
            read_points( argv[i], std::back_inserter(points));
        }
    }
    if ( read_from_stdin)
        read_points( std::cin, std::back_inserter(points));
    std::list<Point> hull;
    incr_convex_hull( points.begin(), points.end(), std::back_inserter( hull));
    if ( ! quiet) {
        for (std::list<Point>::iterator i = hull.begin(); i != hull.end();++i){
            // write points including their index for better clarity.
            std::cout << 'p' << 1 + std::distance( points.begin(), 
                                std::find( points.begin(), points.end(), *i))
                      << ": " << std::setprecision(18) << i->x << ' ' 
                              << std::setprecision(18) << i->y << '\n';
        }
    }
    return 0;
}

