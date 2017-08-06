// Copyright 2003, 2004, 2006 Lutz Kettner, Max-Planck-Institute 
// fuer Informatik, Saarbruecken (Germany).
// Distributed under the Boost Software License, Version 1.0. (See accompany-
// ing file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//
// This file is part of 'Classroom Examples of Robustness Problems in Geometric
// Computations', <http://www.mpi-sb.mpg.de/~kettner/proj/NonRobust/>.
//
// points.h
// 2d points with double coordinates, predicates, file IO support

#ifndef POINTS_H
#define POINTS_H

#include <iostream> 
#include <iomanip>  // for stream output precision
#include <cstdlib>  // for random numbers

// If CGAL is used as well (and one of its header files is included first 
// in the main program source) the predicates use an exact number type
// and cross check the float results against the exact (correct) result.
#ifdef  CGAL_VERSION
#include <CGAL/MP_Float.h>
#endif

// verbose trace for predicates: enables checking against exact predicates
// (only effective if CGAL is used).
static bool verbose_pred = false;

// verbose trace for predicate summary: lists point index and predicate result
static bool verbose_pred_summary = false;

// dummy function to avoid warnings about unused verbose flags.
inline void dummy_no_warn() {
    (void)verbose_pred;
    (void)verbose_pred_summary;
}

// 2d point class with equality and lexicographic comparison
struct Point {
    double x, y;
    Point() {}
    Point( double xx, double yy) : x(xx), y(yy) {}
    bool operator==( const Point& p) const { return x == p.x && y == p.y; }
    bool operator!=( const Point& p) const { return x != p.x || y != p.y; }
    bool operator< ( const Point& p) const { return x < p.x 
                                                || (x == p.x && y < p.y); }
    bool operator> ( const Point& p) const { return p < (*this); }
    bool operator<=( const Point& p) const { return ! ((*this) > p); }
    bool operator>=( const Point& p) const { return ! ((*this) < p); }
};

// stream output for 2d point in debugging format
std::ostream& operator<<( std::ostream& out, const Point& p) {
    out << "Point( " << std::setprecision(18) << p.x << ", " 
                     << std::setprecision(18) << p.y << ')';
    return out;
}

// stream input for 2d point formatted as two doubles, white space separated
std::istream& operator>>( std::istream& in, Point& p) {
    double x, y;
    in >> x >> y;
    if ( in)
        p = Point(x,y);
    return in;
}

// ----------------------------------------------------------------------
// predicates

// Local helper functions to enforce clean IEEE double arithmetic, since 
// function arguments force the proper representation of intermediate values
// as doubles, instead of extended doubles, which would be allowed in 
// arithmetic expressions.
static double multd( double a, double b) { return a * b; }
static double addd(  double a, double b) { return a + b; }

// float_orient: the double precision orientation test.
// Returns +1 for leftturn, 0 for collinear, and -1 for rightturn
inline int f_orientation( const Point& p, const Point& q, const Point& r) {
    // return sign((q.x-p.x) * (r.y-p.y) - (q.y-p.y) * (r.x-p.x));
    double det = addd( multd( q.x-p.x, r.y-p.y), - multd( q.y-p.y, r.x-p.x));
    if ( det < 0.0) return -1;
    if ( det > 0.0) return  1;
    return 0;
}

// orientation: the exact implementation, based on CGAL::MP_Float 
// to have minimal dependence on other packages and speed is not
// relevant here.
// Returns +1 for leftturn, 0 for collinear, and -1 for rightturn
template <class NT>
int x_orientation( const Point& p, const Point& q, const Point& r) {
    NT det = (NT(p.x) - NT(r.x)) * (NT(q.y) - NT(r.y)) - 
             (NT(q.x) - NT(r.x)) * (NT(p.y) - NT(r.y));
    if ( det < NT(0)) return -1;
    if ( det > NT(0)) return  1;
    return 0;
}

// float_orient: checked against exact result if global verbose_pred is true
// Returns +1 for leftturn, 0 for collinear, and -1 for rightturn
inline int orientation( const Point& p, const Point& q, const Point& r) {
    int result = f_orientation(p,q,r);
    #ifdef  CGAL_VERSION
    // checked predicates only available if CGAL is there
    if ( verbose_pred) {
        static const char* s = "-0+";
        int xresult = x_orientation<CGAL::MP_Float>(p,q,r);
        std::cerr << "    Orient( " << p << ",\n"
                  << "            " << q << ",\n"
                  << "            " << r << ")\n"
                  << "      = " << s[result+1] << ' ' << s[xresult+1] 
                  << std::endl;
    }
    #endif // CGAL_VERSION
    return result;
}


// f_extended: the double precision extended test
// Returns true if r is on the extension of the ray starting in q in
// the direction q-p, i.e., if (q-p)*(r-q) >= 0, and false otherwise
inline bool f_extended( const Point& p, const Point& q, const Point& r) {
    // return ((q.x-p.x) * (r.x-q.x) >= (p.y-q.y) * (r.y-q.y));
    return (multd( q.x-p.x, r.x-q.x) >= multd( p.y-q.y, r.y-q.y));
}

// extended: the exact implementation
// Returns true if r is on the extension of the ray starting in q in
// the direction q-p, i.e., if (q-p)*(r-q) >= 0, and false otherwise
template <class NT>
int x_extended( const Point& p, const Point& q, const Point& r) {
    return ((NT(q.x)-NT(p.x)) * (NT(r.x)-NT(q.x)) 
         >= (NT(p.y)-NT(q.y)) * (NT(r.y)-NT(q.y)));
}

// float extended:  checked against exact result if global verbose_pred is true
// Returns true if r is on the extension of the ray starting in q in
// the direction q-p, i.e., if (q-p)*(r-q) >= 0, and false otherwise
inline int extended( const Point& p, const Point& q, const Point& r) {
    bool result = f_extended(p,q,r);
    #ifdef  CGAL_VERSION
    // checked predicates only available if CGAL is there
    if ( verbose_pred) {
        bool xresult = x_extended<CGAL::MP_Float>(p,q,r);
        std::cerr << "    Extended( " << p << ",\n"
                  << "              " << q << ",\n"
                  << "              " << r << ")\n"
                  << "      = " << (result ? "true" : "false") 
                  << (xresult ? " true" : " false") << std::endl;
    }
    #endif // CGAL_VERSION
    return result;
}

// extended_rightturn: Returns true if pqr form a rightturn, or if pqr are 
// collinear and if r is on the extension of the ray starting in q in
// the direction q-p, i.e., if (q-p)*(r-q) >= 0.
// All points must be from the range [first,last), which will be used
// for computing indices in the verbose trace output.
template <class ForwardIterator> 
bool extended_rightturn( const Point& p, const Point& q, const Point& r, 
                         ForwardIterator first, ForwardIterator last) {
    int orient = orientation(p,q,r);
    bool result = (orient == -1) || (orient == 0 && extended(p,q,r));
    if ( verbose_pred_summary) {
        std::cerr << "  Extended_rightturn( " 
                  <<   "p" << 1+std::distance( first, std::find( first,last,p))
                  << ", p" << 1+std::distance( first, std::find( first,last,q))
                  << ", p" << 1+std::distance( first, std::find( first,last,r))
                  << ") == " << (result ? "true " : "false");
        #ifdef  CGAL_VERSION
        // checked predicates only available if CGAL is there
        int  xorient = x_orientation<CGAL::MP_Float>(p,q,r);
        bool xresult = (xorient == -1) || (xorient == 0 && 
                                           x_extended<CGAL::MP_Float>(p,q,r));
        std::cerr << (xresult ? " (true  exact)" : " (false exact)");
        #endif // CGAL_VERSION
        std::cerr << std::endl;
    }
    return result;
}


// ----------------------------------------------------------------------
// algorithms

// Creates n random points in the unit disc and writes them to 'points'.
// Returns value of result output iterator 'points' after filling.
template <class OutputIterator>
OutputIterator random_fill( int n, OutputIterator points) {
    register double x = 0.0;
    register double y = 0.0;
    int i = n;
    while ( i-- > 0) {
        do {
            x = double( std::rand()) / double( RAND_MAX) * 2.0 - 1.0;
            y = double( std::rand()) / double( RAND_MAX) * 2.0 - 1.0;
        } while ( x*x + y*y >= 0.999999);
        *points++ = Point( x, y);
    }
    return points;
}

// Returns true if the endianness of the current platform is little endian,
// as, for example, Intel processors.
inline bool check_little_endian() {
    int i = 1;
    char *p = reinterpret_cast<char*>( &i);
    return (p[0] == 1); // Lowest address contains the least significant byte
}

// Store information about endianness once.
static bool is_little_endian = check_little_endian();

// Change endianness of a double value:
void swap_endianness( double& d) {
    char *p = reinterpret_cast<char*>( &d);
    std::swap( p[0], p[7]);
    std::swap( p[1], p[6]);
    std::swap( p[2], p[5]);
    std::swap( p[3], p[4]);
}

// Reads a list of points from a stream. Returns number of points read.
template <class OutputIterator>
int read_points( std::istream& in, OutputIterator points) {
    int n = 0;
    char c;
    in.get(c);
    if ( ! in)
        return 0;
    if ( c == 'B') { // binary file format
        // read remaining 7 chars before binary data starts
        for ( int i = 0; i < 7; ++i)
            in.get(c);
        // sanity check for the second 'B' in the magic header "BINARY\nB"
        if ( ! in || c != 'B') {
            std::cerr << "Error in header, cannot read binary file."
                      << std::endl;
            return 0;
        }          
        double d[2]; // array to read two double values at once
        do {
            in.read( reinterpret_cast<char*>( d), 16);
            if ( ! in)
                break;
            if ( ! is_little_endian) {
                swap_endianness(d[0]);
                swap_endianness(d[1]);
            }
            *points++ = Point( d[0], d[1]);
            ++n;
        } while (true);
    } else { // ASCII file format
        in.putback(c);
        Point p;
        while ( in >> p) {
            *points++ = p;
            ++n;
        }
    }
    return n;
}

// Reads a list of points from a file. Returns number of points read.
template <class OutputIterator>
int read_points( const char* filename, OutputIterator points) {
    std::ifstream in( filename, std::ios::binary);
    if ( ! in) {
        std::cerr << "Error: cannot open file `" << filename <<
                     "' for reading." << std::endl;
        return 0;
    }
    return read_points( in, points);
}

// Copies a sequence of points to a sequence of exact points of type XPoint.
// Uses following functor internally.
template <class XPoint>
struct Mk_point {
    XPoint operator()( const Point& p) { return XPoint( p.x, p.y); }
};
template <class InputIterator, class Container>
void copy_to_exact( InputIterator first, InputIterator last, Container& cont){
    typedef typename Container::value_type XPoint;
    std::transform( first, last, std::back_inserter(cont), Mk_point<XPoint>());
}


#endif // POINTS_H
