// Copyright 2003, 2004, 2006 Lutz Kettner, Max-Planck-Institute 
// fuer Informatik, Saarbruecken (Germany).
// Distributed under the Boost Software License, Version 1.0. (See accompany-
// ing file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//
// This file is part of 'Classroom Examples of Robustness Problems in Geometric
// Computations', <http://www.mpi-sb.mpg.de/~kettner/proj/NonRobust/>.
//
// fp_scope.C
// Shows the result of the floating-point (fp) orientation test around a 
// small neighborhood of one point visualized as a ppm-image of the
// 2D grid of floating point (double) numbers.
// Main program for visualization and experiments, not based on CGAL.

// Note: This is a stripped down version of fp_scope_ext.C (which is for 
// expert use only). This program can serve as starting point in class
// to repeat our visualization experiments. This program is on purpose
// short and very easy to allow quick understanding and then changes and
// further experiments.

#include <iostream>
#include <iomanip>
#include <fstream>
#include <vector>

#include <points.h>
#include <ppm_image.h>
#include <ieee_double.h>

using std::cerr;
using std::endl;

// Type for storing the input points (will only be exactly three points here)
typedef std::vector<Point> Point_vector;

// Global program options
const char* output_filename = "fp_image.ppm";

// Image dimensions
unsigned int max_x  = 256;
unsigned int max_y  = 256;

// Type of PPM image and RGB pixel
typedef Ppm_image<unsigned char> Image;
typedef Ppm_pixel<unsigned char> Pixel;

// Color scheme used to color orientation test results indexed from 0..2
Pixel color_scheme[3] = {
    Pixel( 255,  64,  64), // negative det  red
    Pixel( 255, 255,  64), // zero     det  yellow
    Pixel(  64,  64, 255)  // positive det  blue
};

// Returns +1 for leftturn, 0 for collinear, and -1 for rightturn.
inline int float_orient( const Point& p, const Point& q, const Point& r) {
    double det = addd( multd( q.x-p.x, r.y-p.y), -multd( q.y-p.y, r.x-p.x));
    if ( det < 0.0) return -1;
    if ( det > 0.0) return  1;
    return 0;
}

// Draw the image around the point p, which is also the pivot in float_orient.
// Note that for mapping coordinate axes to the image addressing conventions
// we reverse the sign on the y-ccordinates in this computation.
void draw_image( Point p, Point q, Point r, Image& image) {
    for ( unsigned int y = 0; y < max_y; ++y) {
        for ( unsigned int x = 0; x < max_x; ++x) {
            Point pp(p.x + Mantissa(x), p.y + Mantissa(y));
            image[max_y - 1 - y][x] = color_scheme[1 + float_orient(pp, q, r)];
        }
    }
}

// Sanity check: Returns true if points actually convert reliably back and 
// forth from iostreams, i.e., exact_io_conversion is true for all coordinates.
// Prints diagnostic error messages in case of violations.
template <class InputIterator>
bool check_points( InputIterator first, InputIterator last) {
    bool result = true;
    for ( ; first != last; ++first) {
        if ( ! exact_io_conversion(first->x)) {
            result = false;
            cerr << "WARNING: binary/decimal conversion of " 
                 << std::setprecision(18) << first->x << " not exact!" << endl;
        }
        if ( ! exact_io_conversion(first->y)) {
            result = false;
            cerr << "WARNING: binary/decimal conversion of " 
                 << std::setprecision(18) << first->y << " not exact!" << endl;
        }
    }
    return result;
}

// Sanity check: The floating-point grid is not really a grid; it is not
// uniform. Print warning messages if we are not in an area of the fp-grid
// that is uniform, i.e., an overflow of the mantissa occurs in the covered
// range, or x- and y- coordinates have different exponents.
void check_grid( Point p) {
    Point checker( p.x + Mantissa(max_x-1), p.y + Mantissa(max_y-1));
    if ( exponent( checker.x) != exponent( p.x))
        cerr << "WARNING: fp-grid not homogeneous in x-direction." << endl;
    if ( exponent( checker.y) != exponent( p.y))
        cerr << "WARNING: fp-grid not homogeneous in y-direction." << endl;
    if ( exponent( p.x) != exponent( p.y))
        cerr << "WARNING: fp-grid not isotropic." << endl;
}


// Main program: parses a view commandline arguments and handles file IO.
int main( int argc, char* argv[]) {
    if ( argc > 1 && ( strcmp( argv[1], "-h") == 0 
                    || strcmp( argv[1], "--help") == 0)) {
        cerr << 
"Usage: fp_scope [<Options>] [<files> ...]\n"
"       Shows the result of the floating-point (fp) orientation test around\n"
"       a small neighborhood of one point visualized as a ppm-image of the\n"
"       2D grid of floating point (double) numbers.\n"
"       Reads points from input files (or stdin). Writes fp_image.ppm file.\n"
"Options: defaults in []\n"
"    -h/--help:     this message\n"
"    -o <filename>: output filename for ppm result image [\"fp_image.ppm\"]\n"
"    -dim <n>:      dimension of the picture [256]\n"
"    -dimx <n>:     x-dimension of the picture [256]\n"
"    -dimy <n>:     y-dimension of the picture [256]\n";
        return 0;
    }
    Point_vector points;
    bool read_from_stdin = true;
    int n = 0;
    for ( int i = 1; i < argc; ++i) {
        if ( strcmp( argv[i], "-dim") == 0) {
            ++i;
            if ( i < argc)
                n = atoi( argv[i]);
            if ( n < 4 || n > 9000) {
                cerr << "Error: choose parameter <n> for -dim between "
                        "[4,9000]." << endl;
                return 1;
            } else {
                max_x = n;
                max_y = n;
            }
        } else if ( strcmp( argv[i], "-dimx") == 0){
            ++i;
            if ( i < argc)
                n = atoi( argv[i]);
            if ( n < 4 || n > 9000) {
                cerr << "Error: choose parameter <n> for -dimx between "
                        "[4,9000]." << endl;
                return 1;
            } else {
                max_x = n;
            }
        } else if ( strcmp( argv[i], "-dimy") == 0){
            ++i;
            if ( i < argc)
                n = atoi( argv[i]);
            if ( n < 4 || n > 9000) {
                cerr << "Error: choose parameter <n> for -dimy between "
                        "[4,9000]." << endl;
                return 1;
            } else {
                max_y = n;
            }
        } else if ( strcmp( argv[i], "-o") == 0){
            ++i;
            if ( i < argc) {
                output_filename = argv[i];
                if ( strcmp( output_filename, "-") == 0)
                    output_filename = 0;
            } else {
                cerr << "Error: option -o needs a filename as parameter."
                     << endl;
                return 1;
            }
        } else {
            read_from_stdin = false;
            read_points( argv[i], std::back_inserter(points));
        }
    }
    if ( read_from_stdin)
        read_points( std::cin, std::back_inserter(points));
    if ( points.size() != 3) {
        cerr << "ERROR: requires three input points." << endl;
        return 1;
    }

    // some sanity checks
    check_points( points.begin(), points.end());
    check_grid( points.front());

    // prepare and draw image
    Image image( max_x, max_y);
    Point p = points[0];
    Point q = points[1];
    Point r = points[2];
    draw_image( p, q, r, image);

    // write result to file (or std::cout if requested with "-")
    if ( output_filename == 0) {
        std::cout << image;
        if ( ! std::cout) {
            cerr << "ERROR: could not write result image to <cout>." << endl;
            return 1;
        }
    } else {
        std::ofstream out( output_filename);
        out << image;
        if ( ! out) {
            cerr << "ERROR: could not write result image to file `" 
                 << output_filename << "'." << endl;
            return 1;
        }
    }
    return 0;
}
