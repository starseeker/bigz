// Copyright 2003, 2004, 2006 Lutz Kettner, Max-Planck-Institute 
// fuer Informatik, Saarbruecken (Germany).
// Distributed under the Boost Software License, Version 1.0. (See accompany-
// ing file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//
// This file is part of 'Classroom Examples of Robustness Problems in Geometric
// Computations', <http://www.mpi-sb.mpg.de/~kettner/proj/NonRobust/>.
//
// gift_wrapping_hull.h
// Gift wrapping algorithm for 2d convex hull.

#ifndef GIFT_WRAPPING_HULL_H
#define GIFT_WRAPPING_HULL_H

#include <circular_list.h> // for container type in function incr_conv_hull
#include <points.h>        // for predicates on points

// verbose trace for algorithm: inserted points and algorithm control flow
static bool verbose_algo = false;


// wrap_step: Given a start point p on the convex hull, find the next extreme
// point q in counterclockwise order in the range [first,last) such that
// the edge pq wraps the points in [first,last). Returns q.
// Note: this is a maximum search in the circular order of points 
// induced by the total order extended_rightturn(p,q,s) around p, 
// where p stays fixed in this algorithm. It is no problem if points 
// equal to p are also contained in [first,last), since p is the minimal
// element in this order.
template <class Point, class ForwardIterator> 
Point wrap_step( Point p, ForwardIterator first, ForwardIterator last) {
    if (verbose_pred_summary) {
        std::cerr << "WrapStep( p" 
                  <<  1 + std::distance( first, std::find( first, last, p))
                  << ')' << std::endl;
    }
    Point q = p;
    for ( ForwardIterator i = first; i != last; ++i) {
        // note: p will be pivot in the orientation test here
        if ( extended_rightturn( p, q, *i, first, last))
            q = *i;
    }
    return q;
}

// Stores the extreme points of the 2d convex hull of the points in the
// range [first,last) in the output iterator 'result' in counterclockwise
// order. Returns 'result' after storing all extreme points.
template <class ForwardIterator, class OutputIterator> 
OutputIterator
gift_wrapping_hull( ForwardIterator first, ForwardIterator last,
                    OutputIterator  result)
{
    typedef std::iterator_traits<ForwardIterator>  Iterator_traits;
    typedef typename Iterator_traits::value_type   Point;
    if ( first == last)
        return result;
    // find first point and initialize first candidate for wrap_step
    Point p0 = * std::min_element( first, last);
    Point p = p0;
    do {
      if (verbose_algo)
            std::cerr << " p" << 1 + std::distance(first, 
                                 std::find(first,last,p));
        *result++ = p;
        p = wrap_step( p, first, last);
    } while ( p != p0);
    return result;
}

#endif // GIFT_WRAPPING_H
