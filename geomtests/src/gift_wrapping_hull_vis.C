// Copyright 2003, 2004, 2006 Lutz Kettner, Max-Planck-Institute 
// fuer Informatik, Saarbruecken (Germany).
// Distributed under the Boost Software License, Version 1.0. (See accompany-
// ing file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//
// This file is part of 'Classroom Examples of Robustness Problems in Geometric
// Computations', <http://www.mpi-sb.mpg.de/~kettner/proj/NonRobust/>.
//
// gift_wrapping_hull_vis.C
// Gift wrapping algorithm for 2d convex hull.
// Main program for testing, visualization, and demo based on CGAL and LEDA.

#include <CGAL/basic.h>
#include <list>
#include <fstream>
#include <string.h>

#include <points.h>
#include <gift_wrapping_hull.h>
#include <cgal_leda_support.h>

int main( int argc, char** argv) {
    if ( argc > 1 && ( strcmp( argv[1], "-h") == 0 
                    || strcmp( argv[1], "--help") == 0)) {
        std::cerr << 
"Usage: gift_wrapping_hull_vis [<Options>] [<files> ...]\n"
"    2D convex hull of the points in <files>. If none is given and \n"
"    option -r is not used, reads points from stdin.\n"
"Options:\n"
"    -h/--help:      this message\n"
"    -ps <filename>: saves drawing in postscript file\n"
"    -novis:         skip visualization in window\n"
"    -square:        set square aspect ratio\n"
"    -v1:            verbose level 1: algorithm control flow\n"
"    -v2:            verbose level 2: orientation tests, symbolic\n"
"    -v3:            verbose level 3: orientation tests, numeric\n"
"    -exact:         draws exact solution in grey.\n"
"    -r/--rand <n>:  creates <n> points randomly chosen from the unit disc\n";
        return 0;
    }
    std::list<Point> points;
    bool exact_solution = false;
    bool read_from_stdin = true;
    bool novis = false;
    bool square = false;
    char* psname = 0;
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
        } else if ( strcmp( argv[i], "-ps") == 0){
            ++i;
            if ( i < argc)
                psname = argv[i];
            else {
                std::cerr << "Error: need filename argument for option -ps" 
                          << std::endl;
                return 1;
            }
        } else if ( strcmp( argv[i], "-novis") == 0){
            novis = true;
        } else if ( strcmp( argv[i], "-square") == 0){
            square = true;
        } else if ( strcmp( argv[i], "-v1") == 0){
            verbose_algo = true;
        } else if ( strcmp( argv[i], "-v2") == 0){
            verbose_pred_summary = true;
        } else if ( strcmp( argv[i], "-v3") == 0){
            verbose_pred = true;
        } else if ( strcmp( argv[i], "-exact") == 0){
            exact_solution = true;
        } else {
            read_from_stdin = false;
            read_points( argv[i], std::back_inserter(points));
        }
    }
    if ( read_from_stdin)
        read_points( std::cin, std::back_inserter(points));
    std::list<Point> hull;
    gift_wrapping_hull( points.begin(), points.end(), 
                        std::back_inserter( hull));
    if ( psname != 0)
        psfile( psname, points, hull, exact_solution, square);
    if ( ! novis)
        visualize( points, hull, exact_solution, square);
    return 0;
}

