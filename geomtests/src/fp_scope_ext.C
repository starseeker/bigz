// Copyright 2003, 2004, 2006 Lutz Kettner, Max-Planck-Institute 
// fuer Informatik, Saarbruecken (Germany).
// Distributed under the Boost Software License, Version 1.0. (See accompany-
// ing file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//
// This file is part of 'Classroom Examples of Robustness Problems in Geometric
// Computations', <http://www.mpi-sb.mpg.de/~kettner/proj/NonRobust/>.
//
// fp_scope_ext.C
// Shows the result of the floating-point (fp) orientation test (or some 
// combination of several of those tests) around a small neighborhood of
// one point visualized as a ppm-image of the 2D grid of floating point 
// (double) numbers. 
// Main program for visualization and experimental research, based on CGAL.

// Note: Expert use only. This program was used to explore many examples
// and options. It is highly modularized and factored into components, and, 
// besides the usage help text, not much documented. It is not intended as 
// primary class material. However, we include it here for the interested 
// users that want to explore more images or to study in detail how we did
// our experiments.

#include <CGAL/basic.h>
#include <CGAL/functional.h>
#include <iostream>
#include <iomanip>
#include <fstream>
#include <vector>

#include <points.h>
#include <ppm_image.h>
#include <ieee_double.h>


// NT: Exact rational number type, needs to support an exact constructor
// from a double value. Possible choices are gmpq, leda_rational, CGAL::
// MP_Float, etc. Select one of the following include/typedef combinations.

#include <CGAL/Gmpq.h>
typedef CGAL::Gmpq NT;

//#include <CGAL/leda_rational.h>
//typedef leda_rational NT;

//#include <CGAL/MP_Float.h>  // did not work recently with the -eps option
//typedef CGAL::MP_Float NT;


using std::cout;
using std::cerr;
using std::endl;
using std::size_t;

using CGAL::bind_1;
using CGAL::bind_2;
using CGAL::bind_3;
using CGAL::bind_4;


typedef std::vector<Point> Point_vector;

// global program options
const char* output_filename = "fp_image.ppm";

unsigned int max_x  = 256;
unsigned int max_y  = 256;
unsigned int border = 2;
bool exact_drawing  = false; // colors pixel next to the exact solution
bool blowup_drawing = false; // enlarged pixel view
bool show_orient    = false; // shows all parameter calls to orient();

bool use_eps = false;
double eps   = 0.0;

int off_p_x  = 0;
int off_p_y  = 0;

enum Drawing_type { ONE_ORIENT, TWO_ORIENTS, FAILURE_4, DRAWING_MAX};
Drawing_type drawing_type = DRAWING_MAX; // selects useful default depending on
                                         // the number of points available

enum Collage_type { SINGLE, TRIPLE, TRIPLE_SMALL, COLLAGE_MAX };
Collage_type collage_type = SINGLE;

enum Pivot_type { PIVOT_P, PIVOT_Q, PIVOT_R, PIVOT_MAX };
Pivot_type pivot_type = PIVOT_P;

enum Orientation_type { ORIENT_DOUBLE, ORIENT_EXT_DOUBLE, ORIENT_EXTM_DOUBLE};
Orientation_type orientation_type = ORIENT_DOUBLE;

typedef Ppm_image<unsigned char> Image;
typedef Ppm_pixel<unsigned char> Pixel;


Pixel line_color(  64, 255,  64);
Pixel grid_color(   0,   0,   0);
Pixel BLACK(   0,   0,   0);

// color array
Pixel color_kandinsky[3] = {
    Pixel( 255,  64,  64), // negative det  red
    Pixel( 255, 255,  64), // zero     det  yellow
    Pixel(  64,  64, 255)  // positive det  blue
};


// color array for two orients, orient 1 soft, orient 2 strong
Pixel color2[9] = {        // orient 1   orient 2
    Pixel(  64,   0,   0), // negative negative det
    //Pixel(   0,   0, 255), // negative negative det
    Pixel(   0,   0,   0), // zero     negative det
    Pixel(   0,  64,   0), // positive negative det
    Pixel( 154,  96,  96), // negative zero     det
    Pixel(  96,  96,  96), // zero     zero     det
    Pixel(  96, 154,  96), // positive zero     det
    Pixel( 255, 196, 196), // negative positive det
    Pixel( 196, 196, 196), // zero     positive det
    Pixel( 255,   0,   0) // positive positive det
//    Pixel( 196, 255, 196) // positive positive det
};

// color array for two orients anded together
// Note: blue regions are slightly different (255 vrs. 254) to make 
// the exact drawing distinguish between these two regions
Pixel color_and[10] = {        // orient 1   orient 2
    Pixel(  64, 255,  64), // negative negative det: inside   o.k.   green
    Pixel( 255, 255,  64), // zero     negative det: boundary o.k.   yellow
    Pixel(  64,  64, 255), // positive negative det: outside  o.k.   blue
    Pixel( 255, 255,  64), // negative zero     det: boundary o.k.   yellow
    Pixel( 255, 255,  64), // zero     zero     det: boundary o.k.   yellow
    Pixel( 144, 144,  32), // positive zero     det: outside  --     ocker
    Pixel(  64,  64, 254), // negative positive det: outside  o.k.   blue
    Pixel( 144, 144,  32), // zero     positive det: outside  --     ocker
    Pixel( 255,  32,  32), // positive positive det: outside  -- !!! red
    Pixel( 160, 160, 160)  // mask value to block out some region
};

// color array for two orients anded together
// Note: regions are slightly different (255 vrs. 254) to make 
// the exact drawing distinguish between these two regions
Pixel color_sf4[10] = {    // lower,   upper    point
    Pixel( 255,  32,  32), // negative negative det: both valid      red
    Pixel(  64, 255,  64), // zero     negative det: upper valid     green
    Pixel(  64, 254,  64), // positive negative det: upper valid     green
    Pixel(  64,  64, 255), // negative zero     det: lower valid     blue
    Pixel( 255, 255,  64), // zero     zero     det: boundary o.k.   yellow
    Pixel( 255, 255,  64), // positive zero     det: outside  --     yellow
    Pixel(  64,  64, 254), // negative positive det: lower valid     blue
    Pixel( 255, 255,  64), // zero     positive det: outside  --     yellow
    Pixel( 255, 255,  64), // positive positive det: outside  o.k.   green
    Pixel( 160, 160, 160)  // mask value to block out some region
};

// returns +1 for leftturn, 0 for collinear, and -1 for rightturn
inline int float_orient( const Point& p, const Point& q, const Point& r) {
    if ( show_orient) {
        cerr << "Pivot: (" << p << "), (" << q << "), (" << r << ")" << endl;
    }
    double det = 0;
    switch (orientation_type) {
    case ORIENT_DOUBLE:
        det = addd( multd(q.x - p.x, r.y - p.y), -multd(q.y - p.y, r.x - p.x));
        break;
    case ORIENT_EXT_DOUBLE:
        det = (q.x-p.x) * (r.y-p.y) - (q.y-p.y) * (r.x-p.x);
        break;
    case ORIENT_EXTM_DOUBLE:
        double lval = (q.x - p.x) * ( r.y - p.y);
        double rval = (q.y - p.y) * ( r.x - p.x);
        if ( lval < rval) return -1;
        if ( lval > rval) return  1;
        return 0;
    }
    if ( det < -eps) return -1;
    if ( det >  eps) return  1;
    return 0;
}

// returns +1 for leftturn, 0 for collinear, and -1 for rightturn
int exact_orient( const Point& p, const Point& q, const Point& r) {
    NT det = (NT(p.x) - NT(r.x)) * (NT(q.y) - NT(r.y)) - 
             (NT(q.x) - NT(r.x)) * (NT(p.y) - NT(r.y));
    if ( det < NT(-eps)) return -1;
    if ( det > NT( eps)) return  1;
    return 0;
}

// functors to parametrize generic drawing function below
struct Orient {
    typedef int result_type;
    typedef CGAL::Arity_tag<3> Arity;
    int operator()( const Point& p, const Point& q, const Point& r) const {
        return 1 + float_orient( p, q, r);
    }
};
struct XOrient {
    typedef int result_type;
    typedef CGAL::Arity_tag<3> Arity;
    int operator()( const Point& p, const Point& q, const Point& r) const {
        return 1 + exact_orient( p, q, r);
    }
};

struct Orient_two_p {
    typedef int result_type;
    typedef CGAL::Arity_tag<5> Arity;
    int operator()( const Point& q,  const Point& r,
                    const Point& q2, const Point& r2, const Point& p) const {
        return 4 + float_orient( p,q,r) + 3 * float_orient( p,q2,r2);
    }
};
struct XOrient_two_p {
    typedef int result_type;
    typedef CGAL::Arity_tag<5> Arity;
    int operator()( const Point& q,  const Point& r,
                    const Point& q2, const Point& r2, const Point& p) const {
        return 4 + exact_orient( p,q,r) + 3 * exact_orient( p,q2,r2);
    }
};

struct Orient_two_q {
    typedef int result_type;
    typedef CGAL::Arity_tag<5> Arity;
    int operator()( const Point& q,  const Point& r,
                    const Point& q2, const Point& r2, const Point& p) const {
        return 4 + float_orient( q,r,p) + 3 * float_orient( q2,r2,p);
    }
};
struct XOrient_two_q {
    typedef int result_type;
    typedef CGAL::Arity_tag<5> Arity;
    int operator()( const Point& q,  const Point& r,
                    const Point& q2, const Point& r2, const Point& p) const {
        return 4 + exact_orient( q,r,p) + 3 * exact_orient( q2,r2,p);
    }
};

struct Orient_two_r {
    typedef int result_type;
    typedef CGAL::Arity_tag<5> Arity;
    int operator()( const Point& q,  const Point& r,
                    const Point& q2, const Point& r2, const Point& p) const {
        return 4 + float_orient( r,p,q) + 3 * float_orient( r2,p,q2);
    }
};
struct XOrient_two_r {
    typedef int result_type;
    typedef CGAL::Arity_tag<5> Arity;
    int operator()( const Point& q,  const Point& r,
                    const Point& q2, const Point& r2, const Point& p) const {
        return 4 + exact_orient( r,p,q) + 3 * exact_orient( r2,p,q2);
    }
};

struct Orient_sf4 {
    typedef int result_type;
    typedef CGAL::Arity_tag<5> Arity;
    int operator()( const Point& q,  const Point& r,
                    const Point& q2, const Point& r2, const Point& p) const {
        int ox = exact_orient(r,q2,p);
        if ( ox == 0 
             || float_orient(r,q2,p) != ox 
             || float_orient(q2,p,r) != ox
             || float_orient(p,r,q2) != ox)
            return 9;
        return 4 + float_orient( p,r,q) + 3 * float_orient( q2,p,q);
    }
};

struct XOrient_sf4 {
    typedef int result_type;
    typedef CGAL::Arity_tag<5> Arity;
    int operator()( const Point& q,  const Point& r,
                    const Point& q2, const Point& r2, const Point& p) const {
        return 4 + exact_orient( p,r,q) + 3 * exact_orient( q2,p,r2);
    }
};

void add_exact_geometry( Image& image, const Image& exact, 
                         Pixel color = line_color) {
    if ( ! exact_drawing || blowup_drawing)
        return;
    assert( image.width()  == exact.width());
    assert( image.height() == exact.height());
    for ( size_t y = 0; y < image.height(); ++y) {
        for ( size_t x = 0; x < image.width(); ++x) {
            bool bounds = false;
            // check 4 neighborhood, exclude image boundaries
            Pixel pix = exact[y][x];
            if ( pix != BLACK) {
                if ( y != 0 && (x < max_x || y != max_y/2) 
                     && pix != exact[y-1][x] && BLACK != exact[y-1][x])
                    bounds = true;
                if ( y != (image.height()-1) 
                     && (x < max_x || y != (max_y/2-1))
                     && pix != exact[y+1][x] && BLACK != exact[y+1][x] )
                    bounds = true;
                if ( x != 0 && x != max_x && pix != exact[y][x-1]
                    && BLACK != exact[y][x-1])
                    bounds = true;
                if ( x != (image.width()-1) && x != (max_x-1) 
                     && pix != exact[y][x+1] && BLACK != exact[y][x+1])
                    bounds = true;
                if ( bounds)
                    image[y][x] = color;
            }
        }
    }
}

void blowup( Image& result, const Image& image, const Image& exact,
             size_t sx = 3, size_t sy = 3, // cell width
             size_t lx = 1, size_t ly = 1) // grid line width
{
    if ( ! blowup_drawing)
        return;
    assert( image.width()  == exact.width());
    assert( image.height() == exact.height());
    size_t max_x = image.width();
    size_t max_y = image.height();
    size_t dx = sx + lx;
    size_t dy = sy + ly;
    Image res( max_x * dx + lx, max_y * dy + ly, grid_color);
    size_t res_y = ly;
    for ( size_t y = 0; y < image.height(); ++y, res_y += dy) {
        size_t res_x = lx;
        for ( size_t x = 0; x < image.width(); ++x, res_x += dx) {
            Pixel pix = image[y][x];
            for ( size_t yy = 0; yy < sy; ++yy) {
                for ( size_t xx = 0; xx < sx; ++xx) {
                    res[res_y+yy][res_x+xx] = pix;
                }
            }
            if ( exact_drawing) {
                // check upper left part of 4 neighborhood, color grid edges
                // exclude image boundaries
                Pixel pix = exact[y][x];
                if ( pix != BLACK) {
                    if ( y != 0 && (x < max_x || y != max_y/2) 
                         && pix != exact[y-1][x] && BLACK != exact[y-1][x]) {
                        for ( size_t yy = 0; yy < ly; ++yy) {
                            for ( size_t xx = 0; xx < lx + dx; ++xx) {
                                res[res_y-ly+yy][res_x-lx+xx] = line_color;
                            }
                        }
                    }
                    if ( x != 0 && x != max_x && pix != exact[y][x-1]
                        && BLACK != exact[y][x-1]) {
                        for ( size_t yy = 0; yy < ly + dy; ++yy) {
                            for ( size_t xx = 0; xx < lx; ++xx) {
                                res[res_y-ly+yy][res_x-lx+xx] = line_color;
                            }
                        }
                    }
                }
            }
        }
    }
    result.swap( res);
}

void check_grid( Point p) {
    Point checker( p.x + Mantissa(max_x-1), p.y + Mantissa(max_y-1));
    if ( exponent( checker.x) != exponent( p.x))
        cerr << "WARNING: fp-grid not homogeneous in x-direction." << endl;
    if ( exponent( checker.y) != exponent( p.y))
        cerr << "WARNING: fp-grid not homogeneous in y-direction." << endl;
    if ( exponent( p.x) != exponent( p.y))
        cerr << "WARNING: fp-grid not isotropic." << endl;
}

// generic draw function for one image, we reverse the y-direction here
template <class Orientation, class XOrientation>
void draw_image( Point p, Image& image, Image& x_image,
                 unsigned int off_x, unsigned int off_y,
                 unsigned int dim_x, unsigned int dim_y,
                 Pixel* color, Orientation float_orient, 
                 XOrientation exact_orient) {
    off_y = image.height() - 1 - off_y;
    for ( unsigned int y = 0; y < dim_y; ++y) {
        for ( unsigned int x = 0; x < dim_x; ++x) {
            Point pp(p.x + Mantissa(x), p.y + Mantissa(y));
            image[ off_y - y][ off_x + x] = color[ float_orient( pp)];
            if ( exact_drawing) {
                x_image[ off_y - y][ off_x + x] = color[ exact_orient(pp)];
            }
        }
    }
}

void single_orient(Point_vector& points, Point p, Image& image, Image& x_image,
                   unsigned int off_x, unsigned int off_y,
                   unsigned int dim_x, unsigned int dim_y,
                   Pixel* color, Pivot_type pivot) {
    if ( points.size() < 2) {
        cerr << "Error: need at least two points to do single_orient drawing."
             << endl;
        return;
    }
    Point q = points[1];
    Point r = points[2];
    switch ( pivot) { 
    case PIVOT_P:
        draw_image( p, image, x_image, off_x, off_y, dim_x, dim_y, color,
                    bind_2( bind_3(  Orient(), r), q),
                    bind_2( bind_3( XOrient(), r), q));
        break;
    case PIVOT_Q:
        draw_image( p, image, x_image, off_x, off_y, dim_x, dim_y, color,
                    bind_1( bind_2(  Orient(), r), q),
                    bind_1( bind_2( XOrient(), r), q));
        break;
    case PIVOT_R:
        draw_image( p, image, x_image, off_x, off_y, dim_x, dim_y, color,
                    bind_1( bind_3(  Orient(), q), r),
                    bind_1( bind_3( XOrient(), q), r));
        break;
    default:
        break;
    }
}

void two_orients( Point_vector& points, Point p, Image& image, Image& x_image,
                  unsigned int off_x, unsigned int off_y,
                  unsigned int dim_x, unsigned int dim_y,
                  Pixel* color, Pivot_type pivot) {
    if ( points.size() < 4) {
        cerr << "Error: need at least four points to do two_orients drawing."
             << endl;
        return;
    }
    Point q  = points[1];
    Point r  = points[2];
    Point q2 = points[3];
    Point r2 = points[4];
    switch ( pivot) { 
    case PIVOT_P:
        draw_image( p, image, x_image, off_x, off_y, dim_x, dim_y, color,
                    bind_1(bind_2(bind_3(bind_4( Orient_two_p(),r2),q2),r),q),
                    bind_1(bind_2(bind_3(bind_4(XOrient_two_p(),r2),q2),r),q));
        break;
    case PIVOT_Q:
        draw_image( p, image, x_image, off_x, off_y, dim_x, dim_y, color,
                    bind_1(bind_2(bind_3(bind_4( Orient_two_q(),r2),q2),r),q),
                    bind_1(bind_2(bind_3(bind_4(XOrient_two_q(),r2),q2),r),q));
        break;
    case PIVOT_R:
        draw_image( p, image, x_image, off_x, off_y, dim_x, dim_y, color,
                    bind_1(bind_2(bind_3(bind_4( Orient_two_r(),r2),q2),r),q),
                    bind_1(bind_2(bind_3(bind_4(XOrient_two_r(),r2),q2),r),q));
        break;
    default:
        break;
    }
}

void failure_4( Point_vector& points, Point p, Image& image, Image& x_image,
                unsigned int off_x, unsigned int off_y,
                unsigned int dim_x, unsigned int dim_y,
                Pixel* color, Pivot_type pivot) {
    if ( points.size() < 4) {
        cerr << "Error: need at least four points to do failure_4 drawing."
             << endl;
        return;
    }
    Point q  = points[1];
    Point r  = points[2];
    Point q2 = points[3];
    Point r2 = points[4];
    draw_image( p, image, x_image, off_x, off_y, dim_x, dim_y, color,
                bind_1(bind_2(bind_3(bind_4( Orient_sf4(),r2),q2),r),q),
                bind_1(bind_2(bind_3(bind_4(XOrient_sf4(),r2),q2),r),q));
}

template <class Type_fct>
void draw_collage( Point_vector& points, Point p, Pixel* color,
                   Image& result, Type_fct fct){
    check_grid( p);
    if ( show_orient)
        cerr << "p = " << p << endl; // some additional trace output 
    unsigned int width  = max_x + 2*border;
    unsigned int height = max_y + 2*border;
    if ( collage_type == TRIPLE) {
        width  = 3*max_x + 4*border;
        height = max_y + 2*border;
    }
    if ( collage_type == TRIPLE_SMALL) {
        width  = 3*max_x/2 + 3*border;
        height = max_y + 3*border;
    }
    Image image(   width, height);
    Image x_image( width, height);
    fct( points, p, image, x_image, border, 
         ((collage_type == TRIPLE_SMALL) ? 3*border/2 : border), 
         max_x, max_y, color, pivot_type);
    if ( collage_type == TRIPLE) {
        fct( points, p, image, x_image, 2*border + max_x, border, max_x, 
             max_y, color, Pivot_type( (int(pivot_type)+1) % PIVOT_MAX));
        fct( points, p, image, x_image, 3*border + 2*max_x, border, max_x,
             max_y, color, Pivot_type( (int(pivot_type)+2) % PIVOT_MAX));
    }
    if ( collage_type == TRIPLE_SMALL) {
        fct( points, p, image, x_image, 2*border + max_x,
             2*border+max_y/2, max_x/2, max_y/2, color,
             Pivot_type( (int(pivot_type)+1) % PIVOT_MAX));
        fct( points, p, image, x_image, 2*border + max_x,
             border, max_x/2, max_y/2, color,
             Pivot_type( (int(pivot_type)+2) % PIVOT_MAX));
    }
    add_exact_geometry( image, x_image, BLACK);
    blowup( image, image, x_image);
    result.swap( image);
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

int main( int argc, char* argv[]) {
    if ( argc > 1 && ( strcmp( argv[1], "-h") == 0 
                       || strcmp( argv[1], "--help") == 0)) {
        cerr << 
"Usage: fp_scope_ext [<Options>] [<files> ...]\n"
"       Shows the result of the floating-point (fp) orientation test (or\n"
"       some combination of several of those tests) around a small\n" 
"       neighborhood of the point p visualized as a ppm-image of the 2D\n"
"       grid of floating point (double) numbers.\n"
"       Reads points from input files (or stdin). Writes fp_image.ppm file.\n"
"Drawing type options: default is selected according to number of points\n"
"    -one:          visualizes one orientation test [3 points]\n"
"    -two:          visualizes two orientation tests [5 points]\n"
"    -sf4:          visualizes search for failure 4, needs 5 points\n"
"Options: defaults in []\n"
"    -h/--help:     this message\n"
"    -o <filename>: output filename for ppm result image [\"fp_image.ppm\"]\n"
"    -pivot <p>:    pivot point: p,q, or r (1., 2., or 3. point) [p]\n"
"    -ext:          use extended double in orientation test\n"
"                   [default is double for all values]\n"
"    -extm:         use extended double in some intermediate values only\n"
"    -exact:        draw also the exact result\n"
"    -eps <eps>:    use tolerance value eps for comparisons instead of 0.0\n"
"    -dim <n>:      dimension of the picture [256]\n"
"    -dimx <n>:     x-dimension of the picture [256]\n"
"    -dimy <n>:     y-dimension of the picture [256]\n"
"    -offx <n>:     x-offset (in mantissa bits) for p [0]\n"
"    -offy <n>:     y-offset (in mantissa bits) for p [0]\n"
"    -border <n>:   pixels used for the black border [2]\n"
"    -c <n>         collage: 1=single, 2=triple, 3=triple with 2 small [1]\n"
"    -blowup:       enlarge drawing to see grid cells\n"
"    -showorient:   show all parameter calls to orient()\n";
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
        } else if ( strcmp( argv[i], "-eps") == 0){
            ++i;
            if ( i < argc)
                eps = atof( argv[i]);
            if ( eps < 0.0 || eps > 1.0) {
                cerr << "Error: choose reasonable eps between 0.0 and 1.0."
                     << endl;
                return 1;
            } else {
                use_eps = true;
            }
        } else if ( strcmp( argv[i], "-one") == 0){
            drawing_type = ONE_ORIENT;
        } else if ( strcmp( argv[i], "-two") == 0){
            drawing_type = TWO_ORIENTS;
        } else if ( strcmp( argv[i], "-sf4") == 0){
            drawing_type = FAILURE_4;
        } else if ( strcmp( argv[i], "-ext") == 0){
            orientation_type = ORIENT_EXT_DOUBLE;
        } else if ( strcmp( argv[i], "-extm") == 0){
            orientation_type = ORIENT_EXTM_DOUBLE;
        } else if ( strcmp( argv[i], "-offx") == 0){
            ++i;
            if ( i < argc)
                off_p_x = atoi( argv[i]);
        } else if ( strcmp( argv[i], "-offy") == 0){
            ++i;
            if ( i < argc)
                off_p_y = atoi( argv[i]);
        } else if ( strcmp( argv[i], "-border") == 0){
            ++i;
            int n = border;
            if ( i < argc)
                n = atoi( argv[i]);
            if ( n < 0 || n > 100) {
                cerr << "Error: choose parameter <n> for -border between "
                        "[0,100]." << endl;
                return 1;
            }
            border = n;
        } else if ( strcmp( argv[i], "-exact") == 0){
            exact_drawing = true;
        } else if ( strcmp( argv[i], "-blowup") == 0){
            blowup_drawing = true;
        } else if ( strcmp( argv[i], "-showorient") == 0){
            show_orient = true;
        } else if ( strcmp( argv[i], "-c") == 0){
            ++i;
            int n = int(collage_type) + 1;
            if ( i < argc)
                n = atoi( argv[i]);
            if ( n < 1 || n > COLLAGE_MAX) {
                cerr << "Error: choose parameter <n> for -c between [1," 
                     << COLLAGE_MAX << "]." << endl;
                return 1;
            }
            collage_type = Collage_type( n-1);
        } else if ( strcmp( argv[i], "-pivot") == 0){
            ++i;
            if ( i < argc) {
                if ( strcmp( argv[i], "p") == 0){
                    pivot_type = PIVOT_P;
                } else if ( strcmp( argv[i], "q") == 0){
                    pivot_type = PIVOT_Q;
                } else if ( strcmp( argv[i], "r") == 0){
                    pivot_type = PIVOT_R;
                } else {
                    cerr << "Error: choose parameter <c> for -pivot as one of "
                            "p,q, or r." << endl;
                    return 1;
                }
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
    if ( points.size() < 3) {
        cerr << "ERROR: need at least three points to do any pictures." <<endl;
        return 1;
    }
    check_points( points.begin(), points.end());
    Image image;
    Point p = points.front();
    p = Point( p.x + Mantissa( off_p_x), p.y + Mantissa( off_p_y));
    if ( drawing_type == DRAWING_MAX) {
        if ( points.size() == 3) {
            drawing_type = ONE_ORIENT;
        } else if ( points.size() == 5) {
            drawing_type = TWO_ORIENTS;
        }
    }
    switch ( drawing_type) {
    case ONE_ORIENT:
        draw_collage( points, p, color_kandinsky, image, single_orient);
        break;
    case TWO_ORIENTS:
        draw_collage( points, p, color_and, image, two_orients);
        break;
    case FAILURE_4:
        draw_collage( points, p, color_sf4, image, failure_4);
        break;
    default:
        cerr << "ERROR: unknown drawing type / no suitable default for the "
                "number of points\n       given to do any picture." << endl;
        return 1;
    }
    if ( output_filename == 0) {
        cout << image;
        if ( ! cout) {
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
