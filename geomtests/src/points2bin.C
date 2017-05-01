// Copyright 2006 Lutz Kettner, Max-Planck-Institute 
// fuer Informatik, Saarbruecken (Germany).
// Distributed under the Boost Software License, Version 1.0. (See accompany-
// ing file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//
// This file is part of 'Classroom Examples of Robustness Problems in Geometric
// Computations', <http://www.mpi-sb.mpg.de/~kettner/proj/NonRobust/>.
//
// points2bin.C
// Program to convert ASCII input files of 2d point data into little-endian
// binary files.

#include <vector>
#include <algorithm>
#include <fstream>
#include <string.h>

#include <points.h>

int main( int argc, char** argv) {
    bool help_option = ( argc > 1 && ( strcmp( argv[1], "-h") == 0 
                                    || strcmp( argv[1], "--help") == 0));
    bool error = (( argc != 3) && ! help_option);
    if ( error )
        std::cerr << "Error: needs two filenames as commandline arguments."
                  << std::endl;
    if ( error || help_option) {
        std::cerr << 
"Usage: points2bin [<Options>] <input-file> <output-file>\n"
"    Converts ASCII input file of 2d point data into little-endian\n"
"    binary output file.\n"
"Options:\n"
            "    -h/--help:      this message\n";
        return (error ? 1 : 0);
    }
    std::vector<Point>  points;
    read_points( argv[1], std::back_inserter(points));
    std::ofstream out( argv[2], std::ios::binary);
    if ( ! out) {
        std::cerr << "Error: cannot open file `" << argv[2] <<
                     "' for writing." << std::endl;
        return 2;
    }
    out << "BINARY\nB"; // 8 bytes fixed magic header
    for (std::vector<Point>::iterator i = points.begin(); i!=points.end();++i){
        double d = i->x;
        if ( ! is_little_endian)
            swap_endianness(d);
        out.write( reinterpret_cast<char*>( &(d)), 8);
        d = i->y;
        if ( ! is_little_endian)
            swap_endianness(d);
        out.write( reinterpret_cast<char*>( &(d)), 8);
    }
    if ( ! out) {
        std::cerr << "Error: could not write to file `" << argv[2] << "'." 
                  << std::endl;
        return 2;
    }
    return 0;
}

