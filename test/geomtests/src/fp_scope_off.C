// Copyright 2006 Lutz Kettner, Max-Planck-Institute 
// fuer Informatik, Saarbruecken (Germany).
// Distributed under the Boost Software License, Version 1.0. (See accompany-
// ing file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//
// This file is part of 'Classroom Examples of Robustness Problems in Geometric
// Computations', <http://www.mpi-sb.mpg.de/~kettner/proj/NonRobust/>.
//
// fp_scope_off.C
// Shows the result of the floating-point (fp) orientation test around a 
// small neighborhood of one point visualized as an OFF 3d mesh file of
// the 2D grid of floating point (double) numbers. Writes fp_mesh.off file.
// Main program for visualization and experiments, not based on CGAL.

#include <iostream>
#include <iomanip>
#include <fstream>
#include <vector>
#include <algorithm>
#include <float.h>

#include <points.h>
#include <ieee_double.h>

using std::cerr;
using std::endl;

char* output_filename = "fp_mesh.off";
bool  use_walls       = true;

// Type for storing the input points (will only be exactly three points here)
typedef std::vector<Point> Point_vector;

// Image dimensions
const unsigned int max_x  = 256;
const unsigned int max_y  = max_x;
const double       max_z  = (max_x + max_y) / 4.0;

double scale_z = 1.0;
double max_val = DBL_MIN;
double min_val = DBL_MAX;

double M[max_y][max_x];        // array of fp mantissa value per grid point
double P[max_y+1][max_x+1][5]; // array of corner points values per grid point
                               // DBL_MAX marks end of corners
int    N[max_y+1][max_x+1];    // number points


// Color scheme used to color orientation test results indexed from 0..3
// Now changed color scheme to blend continuously from red over yellow to blue.
int color_scheme[4][3] = {
    { 255,  64,  64}, // red
    { 255, 255,  64}, // yellow
    {  64,  64, 255}, // blue
    {  64, 255,  64}  // green
};
int yellow[3] = { 255, 255,  64};
int blend_red[2][3] = {
    {  96,   0,   0},  // dark red
    { 255, 196,  64}   // yellow-red, light
};
int blend_blue[2][3] = {
    {   0,   0,  96},  // dark blue
    {  64, 196, 255}   // yellow-blue, light
};

double F[max_x*max_y];
int max_F = 0;

// Returns the orientation test determinant
// value, which is > 0 for leftturn, 0 for collinear, and < 0 for rightturn.
inline double float_orient( const Point& p, const Point& q, const Point& r) {
    double det = addd( multd( q.x-p.x, r.y-p.y), -multd( q.y-p.y, r.x-p.x));
    return det;
}

// Initialize M, P, and N
// Returns number of points
int init_terrain( Point p, Point q, Point r) {
    // init M
    for ( unsigned int y = 0; y < max_y; ++y) {
        for ( unsigned int x = 0; x < max_x; ++x) {
            Point pp(p.x + Mantissa(x), p.y + Mantissa(y));
            M[y][x] = float_orient(pp, q, r);
            F[x+y*max_x] = M[y][x];
            if ( M[y][x] > max_val)
                max_val = M[y][x];
            if ( M[y][x] < min_val)
                min_val = M[y][x];
        }
    }
    double diff = max_val - min_val;
    int bound = 0;
    if ( diff > 0.0) {
        // double scale_z until (max_val - min_val) * scale_z > max_z
        while ( diff * scale_z < max_z) {
            scale_z = scale_z * 2.0;
            if ( ++ bound > 200)
                break;
        }
    }
    // process color indices
    std::sort( F, F+max_x*max_y);
    max_F = (std::unique( F, F+max_x*max_y) - F);
    // set P to DOUBLE_MAX
    for ( unsigned int y = 0; y < max_y+1; ++y) {
        for ( unsigned int x = 0; x < max_x+1; ++x) {
            for ( unsigned int i = 0; i != 5; ++i) {
                P[y][x][i] = DBL_MAX;
            }
        }
    }
    // assign proper values of M to P
    for ( unsigned int y = 0; y < max_y; ++y) {
        for ( unsigned int x = 0; x < max_x; ++x) {
            P[y]  [x]  [0] = M[y][x];
            P[y]  [x+1][1] = M[y][x];
            P[y+1][x]  [2] = M[y][x];
            P[y+1][x+1][3] = M[y][x];
        }
    }
    // sort and unique P, number P
    int n = 0;
    for ( unsigned int y = 0; y < max_y+1; ++y) {
        for ( unsigned int x = 0; x < max_x+1; ++x) {
            std::sort( P[y][x], P[y][x]+5);
            double* ptr = std::unique( P[y][x], P[y][x]+5);
            for ( ; ptr != P[y][x]+5; ++ptr)
                *ptr = DBL_MAX;
            N[y][x] = n;
            n += std::count_if( P[y][x], P[y][x]+5, std::bind2nd( 
                                    std::not_equal_to<double>(), DBL_MAX));
        }
    }
    return n;
}

// Return the rank of 'val' in the array F of all possible values of 'val'.
// Used in old way of color scheme handling.
int col_idx( double val) {
    return (std::find( F, F+max_F, val) - F) % 4;
}

// Return a float from -1 over 0 to +1 representing a continuousthe rank of 'val' in the array F of all possible values of 'val'.
// Used in old way of color scheme handling.
void print_blended_col( std::ostream& out, double val) {
    if ( val == 0.0) { // yellow
        out << yellow[0] << ' ' << yellow[1] << ' ' << yellow[2] << '\n';
    } else if ( val < 0.0) { // blended red color
        double s = val / min_val;
        out << int( s * blend_red[0][0] + (1-s) * blend_red[1][0]) << ' ' 
            << int( s * blend_red[0][1] + (1-s) * blend_red[1][1]) << ' ' 
            << int( s * blend_red[0][2] + (1-s) * blend_red[1][2]) << '\n';
    } else { // blended blue color
        double s = val / max_val;
        out << int( s * blend_blue[0][0] + (1-s) * blend_blue[1][0]) << ' ' 
            << int( s * blend_blue[0][1] + (1-s) * blend_blue[1][1]) << ' ' 
            << int( s * blend_blue[0][2] + (1-s) * blend_blue[1][2]) << '\n';
    }
}

// Return number of facets
int number_of_facets() {
    int n = max_x * max_y; // that's all horizontal squares
    // add one to n for each neighboring pair of values that differ
    // return n; // use if only horizontal facets are of interest
    if ( use_walls) {
        for ( unsigned int y = 0; y < max_y; ++y) {
            for ( unsigned int x = 0; x < max_x; ++x) {
                if ( y < max_y - 1 && M[y][x] != M[y+1][x])
                    ++n;
                if ( x < max_x - 1 && M[y][x] != M[y][x+1])
                    ++n;
            }
        }
    }
    return n;
}

// Get point index
inline int idx( int y, int x, double val) {
    if ( y < 0 || y > int(max_y) || x < 0 || x > int(max_x)) {
        cerr << "Out of bounds " << x << ' ' << y << ' ' << val << endl;
        exit(1);
    }
    if ( P[y][x][0] == val)
        return N[y][x];
    else if ( P[y][x][1] == val)
        return N[y][x]+1;
    else if ( P[y][x][2] == val)
        return N[y][x]+2;
    else if ( P[y][x][3] == val)
        return N[y][x]+3;
    cerr << "Wrong Index " << x << ' ' << y << ' ' << val << endl;
    exit(1);
}

// Write OFF File
void write_off( int vertices, int facets, std::ostream& out) {
    out << "OFF\n" << vertices << ' ' << facets << " 0\n";
    out << "# vertices\n";
    for ( unsigned int y = 0; y < max_y+1; ++y) {
        for ( unsigned int x = 0; x < max_x+1; ++x) {
            for ( unsigned int i = 0; P[y][x][i] != DBL_MAX; ++i) {
                out << x << ' ' << y << ' ' << std::setprecision(18) 
                    << (P[y][x][i]*scale_z) << '\n';
            }
        }
    }
    out << "# facets\n";
    for ( unsigned int y = 0; y < max_y; ++y) {
        for ( unsigned int x = 0; x < max_x; ++x) {
            double v = M[y][x];
            out << "4  " << idx(y,x,v) << ' ' << idx(y,x+1,v) << ' ' 
                << idx(y+1,x+1,v) << ' ' << idx(y+1,x,v) << "  ";
            print_blended_col( out, v); // new coloring scheme
            //int* col = color_scheme[col_idx(v)]; // old coloring scheme
            //out << col[0] << ' ' << col[1] << ' ' << col[2] << '\n';
            if ( use_walls) {
                double w = M[y+1][x];
                if ( y < max_y - 1 && v != w) {
                    int iv = idx(y+1,x,v);
                    int iw = idx(y+1,x,w);
                    int jv = idx(y+1,x+1,v);
                    int jw = idx(y+1,x+1,w);
                    if ( v < w) {
                        assert( iv < iw);
                        assert( jv < jw);
                        out << (2+iw-iv+jw-jv) << "  ";
                        for ( int i = jv; i != jw+1; ++i)
                            out << i << ' ';
                        for ( int i = iw; i != iv-1; --i)
                            out << i << ' ';
                    } else {
                        assert( iv > iw);
                        assert( jv > jw);
                        out << (2-iw+iv-jw+jv) << "  ";
                        for ( int i = jv; i != jw-1; --i)
                            out << i << ' ';
                        for ( int i = iw; i != iv+1; ++i)
                            out << i << ' ';
                    }
                    out << "168 168 168\n";
                }
                w = M[y][x+1];
                if ( x < max_x - 1 && v != w) {
                    int iv = idx(y  ,x+1,v);
                    int iw = idx(y  ,x+1,w);
                    int jv = idx(y+1,x+1,v);
                    int jw = idx(y+1,x+1,w);
                    if ( v < w) {
                        assert( iv < iw);
                        assert( jv < jw);
                        out << (2+iw-iv+jw-jv) << "  ";
                        for ( int i = jw; i != jv-1; --i)
                            out << i << ' ';
                        for ( int i = iv; i != iw+1; ++i)
                            out << i << ' ';
                    } else {
                        assert( iv > iw);
                        assert( jv > jw);
                        out << (2-iw+iv-jw+jv) << "  ";
                        for ( int i = jw; i != jv+1; ++i)
                            out << i << ' ';
                        for ( int i = iv; i != iw-1; --i)
                            out << i << ' ';
                    }
                    out << "168 168 168\n";
                }
            }
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
"Usage: fp_scope_off [<Options>] [<files> ...]\n"
"       Shows the result of the floating-point (fp) orientation test around\n"
"       a small neighborhood of one point visualized as an OFF 3d mesh of the\n"
"       2D grid of floating point (double) numbers.\n"
"       Reads points from input files (or stdin). Writes fp_mesh.off file.\n"
"Options: defaults in []\n"
"    -h/--help:     this message\n"
"    -o <filename>: output filename for ppm result image [\"fp_mesh.off\"]\n"
"    -nw:           no vertical walls\n";
        return 0;
    }
    Point_vector points;
    bool read_from_stdin = true;
    for ( int i = 1; i < argc; ++i) {
        if ( strcmp( argv[i], "-o") == 0){
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
        } else if ( strcmp( argv[i], "-nw") == 0){
            use_walls = false;
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

    // prepare and write mesh
    Point p = points[0];
    Point q = points[1];
    Point r = points[2];
    int vertices = init_terrain( p, q, r);
    int facets   = number_of_facets();

    // write result to file (or std::cout if requested with "-")
    if ( output_filename == 0) {
        write_off( vertices, facets, std::cout);
        if ( ! std::cout) {
            cerr << "ERROR: could not write result image to <cout>." << endl;
            return 1;
        }
    } else {
        std::ofstream out( output_filename);
        write_off( vertices, facets, out);
        if ( ! out) {
            cerr << "ERROR: could not write result image to file `" 
                 << output_filename << "'." << endl;
            return 1;
        }
    }
    return 0;
}
