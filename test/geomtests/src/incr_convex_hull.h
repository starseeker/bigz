// Copyright 2003, 2004, 2006 Lutz Kettner, Max-Planck-Institute 
// fuer Informatik, Saarbruecken (Germany).
// Distributed under the Boost Software License, Version 1.0. (See accompany-
// ing file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//
// This file is part of 'Classroom Examples of Robustness Problems in Geometric
// Computations', <http://www.mpi-sb.mpg.de/~kettner/proj/NonRobust/>.
//
// incr_convex_hull.h
// A simple incremental algorithm for 2d convex hull.

#ifndef INCR_CONVEX_HULL_H
#define INCR_CONVEX_HULL_H

#include <circular_list.h> // for container type in function incr_conv_hull
#include <points.h>        // for predicates on points

// verbose trace for algorithm: inserted points and algorithm control flow
static bool verbose_algo = false;

// Scans the range [first,last) until it finds three non-collinear
// points or until the range is exhausted. 
// Returns the iterator 'r' and the convex hull 'hull' with the properties
// that all points in the range [first,r) are contained in 'hull'.
template <class ForwardIterator, class Container> 
ForwardIterator 
find_first_triangle( ForwardIterator first, ForwardIterator last,
                     Container& hull) {
    typedef typename Container::value_type   Point;
    if (first == last)
        return first; // empty hull
    Point p_min = *first;
    Point p_max = p_min;
    while ( ++first != last && *first == p_min) // search 2nd distinct point
        ; // empty stmt
    if ( first == last) { // hull has one point only
        hull.push_back( p_min);
        return first;
    }
    if ( *first < p_min)
        p_min = *first;
    else
        p_max = *first;
    // search 3rd non-collinear point
    while ( ++first != last && orientation( p_min, p_max, *first) == 0) {
        if ( *first < p_min) // update left and right extreme points
            p_min = *first;  // while we are still collinear
        else if ( p_max < *first)
            p_max = *first;
    }
    if ( first == last) { // hull has two points only
        hull.push_back( p_min);
        hull.push_back( p_max);
        return first;
    }
    // found proper triangle, create hull with correct orientation
    // extreme points in counterclockwise (ccw) orientation 
    hull.push_back( p_min);
    if ( orientation( p_min, p_max, *first) > 0) {
        hull.push_back( p_max);
        hull.push_back( *first);
    } else {
        hull.push_back( *first);
        hull.push_back( p_max);
    }
    return ++first;
}

// Stores the extreme points of the 2d convex hull of the points in the
// range [first,last) in the output iterator 'result' in counterclockwise
// order. Returns 'result' after storing all extreme points.
template <class ForwardIterator, class OutputIterator> 
OutputIterator
incr_convex_hull( ForwardIterator first, ForwardIterator last,
                  OutputIterator  result)
{
    typedef std::iterator_traits<ForwardIterator>  Iterator_traits;
    typedef typename Iterator_traits::value_type   Point;
    typedef Circular_list<Point>                   Hull;
    typedef typename Hull::iterator                iterator;
    typedef typename Hull::Circulator              Circulator;

    Hull hull; // extreme points in counterclockwise (ccw) orientation 
    // first the degenerate cases until we have a proper triangle
    ForwardIterator i = find_first_triangle( first, last, hull);
    iterator hi = hull.begin();
    if (verbose_algo) {
        std::cerr << "p1 = " << *hi++ << "\np2 = " << *hi++ << "\np3 = " 
                  << *hi++ << std::endl;
    }
    while ( i != last) {
        if (verbose_algo)
            std::cerr << "Inserting p" 
                      << 1+std::distance( first, std::find( first,last, *i))
                      << " = " << *i << std::endl;
        Point p = *i;
        // find visible edge in circular list of vertices of current hull
        Circulator c_source = hull.circulator_begin();
        Circulator c_dest = c_source;
        do {
            c_source = c_dest++;
            if ( orientation( *c_source, *c_dest, p) < 0) {
                if (verbose_algo)
                    std::cerr << "    found  visible edge p"
                              << 1+std::distance( first, 
                                 std::find( first,last, *c_source)) << " - p"
                              << 1+std::distance( first, 
                                 std::find( first,last, *c_dest))
                              << std::endl;
                // found visible edge, find ccw tangent
                Circulator c_succ = c_dest++;
                while ( orientation( *c_succ, *c_dest, p) <= 0) {
                    if (verbose_algo)
                        std::cerr << "    extend visible edge ccw to p" 
                                  << 1+std::distance( first, 
                                     std::find( first,last, *c_dest))
                                  << std::endl;
                    c_succ = c_dest++;
                }
                // find cw tangent
                Circulator c_pred = c_source--;
                while ( orientation( *c_source, *c_pred, p) <= 0) {
                    if (verbose_algo)
                        std::cerr << "    extend visible edge cw to p" 
                                  << 1+std::distance( first, 
                                     std::find( first,last, *c_source))
                                  << std::endl;
                    c_pred = c_source--;
                }
                // c_pred is the first point visible, c_succ the last
                if ( ++c_pred != c_succ)
                    hull.circular_remove( c_pred, c_succ);
                hull.insert( c_succ, p);
                break; // we processed all visible edges
            }
        } while ( c_dest != hull.circulator_begin());
        ++i;
    }
    return std::copy( hull.begin(), hull.end(), result);
}

#endif // INCR_CONVEX_HULL_H
