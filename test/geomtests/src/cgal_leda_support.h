// Copyright 2003, 2004, 2006 Lutz Kettner, Max-Planck-Institute 
// fuer Informatik, Saarbruecken (Germany).
// Distributed under the Boost Software License, Version 1.0. (See accompany-
// ing file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//
// This file is part of 'Classroom Examples of Robustness Problems in Geometric
// Computations', <http://www.mpi-sb.mpg.de/~kettner/proj/NonRobust/>.
//
// cgal_leda_support.h
// CGAL and LEDA support for exact computation, checking, and visualization
// on screen and in PostScript files.

#ifndef CGAL_LEDA_SUPPORT_H
#define CGAL_LEDA_SUPPORT_H

#include <CGAL/basic.h>
#include <CGAL/Exact_predicates_inexact_constructions_kernel.h>
#include <CGAL/convex_hull_2.h>
#include <CGAL/Polygon_2_algorithms.h>
#include <CGAL/convexity_check_2.h>
#include <CGAL/Bbox_2.h>
#include <CGAL/IO/leda_window.h>
#include <CGAL/IO/Ostream_iterator.h>
#include <CGAL/IO/Postscript_file_stream.h>
#include <list>
#include <algorithm>

#include <points.h>


// Types used from CGAL and LEDA
typedef CGAL::Bbox_2                  Bbox;
typedef leda_window                   Window;
typedef CGAL::Postscript_file_stream  Postscript_stream;


// Add support to make the Point class from points.h interoperable with CGAL.
CGAL_BEGIN_NAMESPACE
// Kernel traits for generic CGAL algorithms
template <> struct Kernel_traits<Point> {
    typedef CGAL::Simple_cartesian<double> Kernel;
};
CGAL_END_NAMESPACE

// Output operator for LEDA window as supported with CGAL
Window& operator<<( Window& w, const Point& p) {
    w.draw_filled_node( p.x, p.y);
    return w;
}

// Output operator for LEDA PostScript output as supported with CGAL
Postscript_stream& operator<<( Postscript_stream& w, const Point& p) {
    CGAL::Simple_cartesian<double>::Point_2 q(p.x, p.y);
    w << q;
    // Following did not work as aspected, no black circle around filled node.
    //w.draw_filled_node( p.x, p.y); 
    return w;
}

// Returns the bounding box of a sequence of points, used in visualization
template <class InputIterator>
Bbox bbox( InputIterator first, InputIterator last) {
    if ( first == last)
        return Bbox( 0.0, 0.0, 0.0, 0.0);
    Bbox box = Bbox( first->x, first->y, first->x, first->y);
    for ( ++first; first != last; ++first)
        box = box + Bbox( first->x, first->y, first->x, first->y);
    return box;
}

template <class Canvas, class Container>
void draw_in_window( Canvas& window, 
                     const Container& points, const Container& hull, 
                     double xmin, double xmax, double ymin, double ymax,
                     bool exact_solution, bool black = false) {
    typedef CGAL::Exact_predicates_inexact_constructions_kernel  XKernel;
    typedef std::list< XKernel::Point_2>          XPoint_list;
    typedef XPoint_list::const_iterator           XPoint_iterator;
    typedef typename Container::const_iterator    Point_iterator;
    typedef CGAL::Ostream_iterator<Point, Canvas> Canvas_iterator;

    // crosshair coordinate system
    window.set_line_width( 1);
    window << CGAL::Color( 128, 128, 128);
    window.draw_segment( 0.0, ymin, 0.0, ymax);
    window.draw_segment( xmin, 0.0, xmax, 0.0);
    window.set_line_width( 2);
    window.set_node_width( 4);

    // exact convex hull
    XPoint_list exact;
    copy_to_exact( points.begin(), points.end(), exact);
    XPoint_list xhull;
    CGAL::convex_hull_2( exact.begin(), exact.end(), 
                         std::back_inserter(xhull));
    cout << points.size() << " input points, " << hull.size()
         << " points on the convex hull, and " << xhull.size()
         << " exactly." << endl;
    XPoint_list hcheck;
    copy_to_exact( hull.begin(), hull.end(), hcheck);
    if ( ! CGAL::is_ccw_strongly_convex_2( hcheck.begin(), hcheck.end()))
        cout << "ERROR: The convex hull is not convex." << endl;
    if ( ! CGAL::is_simple_2( hcheck.begin(), hcheck.end()))
        cout << "ERROR: The convex hull has self-intersections." << endl;
    if ( exact_solution) {
        window << CGAL::Color( 128, 128, 255);
        window.set_line_width( 1);
        for ( XPoint_iterator i = xhull.begin(), j=i++; i != xhull.end(); 
              ++i, ++j) {
            window.draw_segment(CGAL::to_double(i->x()), 
                                CGAL::to_double(i->y()),
                                CGAL::to_double(j->x()),
                                CGAL::to_double(j->y()));
        }
        window.draw_segment( CGAL::to_double(xhull.back().x()),
                             CGAL::to_double(xhull.back().y()),
                             CGAL::to_double(xhull.front().x()),
                             CGAL::to_double(xhull.front().y()));
        window.set_line_width( 2);
    }
    // draw the points and the convex hull
    Canvas_iterator wout( window);
    window << ( black ? CGAL::BLACK : CGAL::WHITE);
    std::copy( points.begin(), points.end(), wout);
    window << CGAL::GREEN;
    for ( Point_iterator i = hull.begin(), j=i++; i != hull.end(); ++i, ++j) {
        window.draw_segment( i->x, i->y, j->x, j->y);
    }
    window.draw_segment( hull.back().x,  hull.back().y, 
                         hull.front().x, hull.front().y);
    window << CGAL::RED;
    std::copy( hull.begin(), hull.end(), wout);
}

// Visualize the point set and its convex hull in a LEDA window.
// Wait for a mouse click before return.
template <class Container>
void visualize( const Container& points, const Container& hull, 
                bool exact_solution, bool square) {
    // always include origin and ensure non-zero dimensions
    Bbox box = bbox( points.begin(), points.end())
             + Bbox(-0.01,-0.01, 0.01, 0.01);
    double xmin = box.xmin();
    double xmax = box.xmax();
    double ymin = box.ymin();
    double ymax = box.ymax();
    double dx = xmax - xmin;
    double dy = ymax - ymin;
    if ( square) {
        if ( dx < dy) { // rescale x-extremes to square window size
            xmin = xmin * dy / dx;
            xmax = xmax * dy / dx;
        } else if ( dx > dy) { // rescale y-extremes to square window size
            ymin = ymin * dx / dy;
            ymax = ymax * dx / dy;
        }
    }
    // add a small border
    double border = (xmax - xmin) / 50.0;
    xmin -= border;
    xmax += border;
    ymin -= border;
    ymax += border;

    // If you whish to magnify a perticular point, this can be done here 
    // manually hardcoded by uncommenting the following lines.
    //xmin = 23.99999;
    //xmax = 24.00001;
    //ymin = 23.99999;
    //ymax = 24.00001;

    dx = xmax - xmin;
    dy = ymax - ymin;
    int width  = 512; // pixel
    int height = int(512.0 * dy / dx + 0.5); // pixel
    if ( height > 512) { // pixel
        height = 512; // pixel
        width  = int( 512.0 * dx / dy + 0.5);
    }
    Window window( width, height, "Convex Hull");
    window.set_icon_label("Convex Hull");
    window.set_icon_pixrect( window.create_pixrect( CGAL::cgal_logo));
    window.init( xmin, xmax, ymin);
    window.display();
    draw_in_window( window, points, hull, xmin, xmax, ymin, ymax, 
                    exact_solution);
    window.read_mouse();
}

// Write a PostScript file with the Visualization
template <class Container>
void psfile( const char* psname,
             const Container& points, const Container& hull,
             bool exact_solution, bool square) {
    // always include origin and ensure non-zero dimensions
    Bbox box = bbox( points.begin(), points.end())
             + Bbox(-0.01,-0.01, 0.01, 0.01);
    double xmin = box.xmin();
    double xmax = box.xmax();
    double ymin = box.ymin();
    double ymax = box.ymax();
    double dx = xmax - xmin;
    double dy = ymax - ymin;
    if ( square) {
        if ( dx < dy) { // rescale x-extremes to square window size
            xmin = xmin * dy / dx;
            xmax = xmax * dy / dx;
        } else if ( dx > dy) { // rescale y-extremes to square window size
            ymin = ymin * dx / dy;
            ymax = ymax * dx / dy;
        }
    }
    // add a small border
    double border = (xmax - xmin) / 50.0;
    xmin -= border;
    xmax += border;
    ymin -= border;
    ymax += border;

    // If you whish to magnify a perticular point, this can be done here 
    // manually hardcoded by uncommenting the following lines.
    //xmin = 23.99999;
    //xmax = 24.00001;
    //ymin = 23.99999;
    //ymax = 24.00001;

    dx = xmax - xmin;
    dy = ymax - ymin;
    double width  = 16.0; // cm
    double height = width / dx * dy; // cm
    if ( height > 22.0) { // cm
        height = 22.0; // cm
        width  = 22.0 / dy * dx;
    }
    Postscript_stream ps( width*40, height*40, psname);
    ps.init( xmin, xmax, ymin);
    ps.set_point_style( leda::circle_point);    
    // bounding frame
    //ps.set_line_width( 1);
    //ps << CGAL::BLACK;
    //ps.draw_segment( xmin, ymin, xmin, ymax);
    //ps.draw_segment( xmax, ymin, xmax, ymax);
    //ps.draw_segment( xmin, ymin, xmax, ymin);
    //ps.draw_segment( xmin, ymax, xmax, ymax);
    draw_in_window( ps, points, hull, xmin, xmax, ymin, ymax, exact_solution,
                    true);
}



#endif // CGAL_LEDA_SUPPORT_H
