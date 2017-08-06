// Copyright 2003, 2004, 2006 Lutz Kettner, Max-Planck-Institute 
// fuer Informatik, Saarbruecken (Germany).
// Distributed under the Boost Software License, Version 1.0. (See accompany-
// ing file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//
// This file is part of 'Classroom Examples of Robustness Problems in Geometric
// Computations', <http://www.mpi-sb.mpg.de/~kettner/proj/NonRobust/>.
//
// circular_list.h
// A standard list enhanced with a circulator and other
// circular list functionality.

#ifndef CIRCULAR_LIST_H
#define CIRCULAR_LIST_H

#include <list>    // derive circular list from the std::list
#include <cassert> // use assertions for precondition checks, etc.

// Circulators behave similar to iterators, but on circular lists
// without a past-the-end position. See the reference manual of CGAL,
// <http://www.cgal.org/> in the Circulator section on details.
// We use a specialized circulator-from-container adaptor here to 
// be independent of CGAL, which would contains a generic adaptor.

// We make sure that if CGAL is used as well, that this implementation
// here is compliant.
#ifdef  CGAL_VERSION
#include <CGAL/circulator.h>
#endif

// This adaptor is used for mutable as well as constant circulators,
// depending on the arguments.
template < class Ctnr, class CtnrPtr, class Iter, class Ref, class Ptr>
class Circulator_for_list {
public:
// TYPES
    typedef Circulator_for_list< Ctnr, CtnrPtr, Iter, Ref, Ptr>  Self;
    typedef Ctnr                                Container;
    typedef CtnrPtr                             Container_pointer;
    typedef Iter                                iterator;
    typedef Ref                                 reference;
    typedef Ptr                                 pointer;
    typedef typename Ctnr::value_type           value_type;
    typedef typename Ctnr::iterator             mutable_iterator;
    typedef typename Ctnr::reference            mutable_reference;
    typedef value_type*                         mutable_pointer;
    typedef Circulator_for_list< Ctnr, Ctnr*, mutable_iterator, 
           mutable_reference, mutable_pointer>  Mutable;
    typedef typename Ctnr::size_type            size_type;
    typedef typename Ctnr::difference_type      difference_type;

#ifndef CGAL_CIRCULATOR_H
    typedef std::bidirectional_iterator_tag     iterator_category;
#else
    typedef CGAL::Bidirectional_circulator_tag  iterator_category;
#endif

private:
    Container_pointer ctnr;
    iterator          i;

public:
// CREATION
    Circulator_for_list()
        : ctnr(0) {}
    Circulator_for_list( Container_pointer c) 
        : ctnr(c), i(c->begin()) {}
    Circulator_for_list( Container_pointer c, iterator j)
        : ctnr(c), i(j) {}
    // depending on the template arguments, this is either a copy constructor,
    // or a constructor that allows the assignment of a mutable circulator
    // to its corresponding constant circulator
    Circulator_for_list( const Mutable& c)
        : ctnr( c.container()), i( c.current_iterator()) {}

// OPERATIONS
    bool operator==( int p) const {
        assert( p == 0);
        return (ctnr == 0) || (ctnr->begin() == ctnr->end());
    }
    bool operator!=( int p) const { return !(*this == p); }
    bool operator==( const Self& c) const { return i == c.i; }
    bool operator!=( const Self& c) const { return !(*this == c); }
    reference  operator*() const {
        assert( ctnr != 0 && i != ctnr->end());
        return *i;
    }
    pointer  operator->() const {
        assert( ctnr != 0 && i != ctnr->end());
        return i.operator->();
    }
    Self& operator++() {
        assert( ctnr != 0 && i != ctnr->end());
        ++i;
        if ( i == ctnr->end())
            i = ctnr->begin();
        return *this;
    }
    Self operator++(int) {
        Self tmp= *this;
        ++*this;
        return tmp;
    }
    Self& operator--() {
        assert( ctnr != 0 && i != ctnr->end());
        if ( i == ctnr->begin())
            i = ctnr->end();
        --i;
        return *this;
    }
    Self operator--(int) {
        Self tmp = *this;
        --*this;
        return tmp;
    }
    iterator           current_iterator() const { return i;}
    Container_pointer  container()        const { return ctnr; }
};


template <class T, class Alloc = std::allocator<T> >
class Circular_list : public std::list<T, Alloc> {
public:
// TYPES
    typedef Circular_list<T, Alloc>       Self;
    typedef std::list<T, Alloc>           Base;
    typedef typename Base::iterator       iterator;
    typedef typename Base::const_iterator const_iterator;
    typedef typename Base::value_type     value_type;
    typedef typename Base::size_type      size_type;
    typedef Circulator_for_list< Self, Self*, iterator, T&, T*> Circulator;
    typedef Circulator_for_list< Self, const Self*, const_iterator, 
                                 const T&, const T*>      Const_circulator;

// CREATION
    Circular_list() {}
    explicit Circular_list( size_type n) : Base(n) {}
    Circular_list( size_type n, const T& t) : Base(n,t) {}
    template <class InputIterator>
    Circular_list(InputIterator f, InputIterator l) : Base(f,l) {}

// OPERATIONS
    Circulator       circulator_begin()      { return Circulator( this); }
    Const_circulator circulator_begin() const {return Const_circulator(this);}

    // Insert a value t before position c.
    void insert( const Circulator& c, const T& t) {
        Base::insert( c.current_iterator(), size_type(1), t);
    }

    // We have to repeat all other insert functions of the std::list although
    // we just forward them to the base class, since name lookup for 'insert'
    // stops now here in this derived class and the other insert functions 
    // would not be available.
    
    // Insert x before pos.
    iterator insert( iterator pos, const T& x) { return Base::insert(pos, x);}

    // Insert the range [f, l) before pos.
    template <class InpIter>
    void insert( iterator pos, InpIter f, InpIter l) { Base::insert(pos,f,l);}

    // Insert n copies of x before pos.    
    void insert( iterator pos, size_type n, const T& x){Base::insert(pos,n,x);}

    // Erase the element at position pos.
    void erase( const Circulator& c)  { Base::erase(c.current_iterator()); }

    // Same as for insert, we have to repeat all erase functions.

    // Erase the element at position pos.
    iterator erase( iterator pos)  { return Base::erase(pos); }

    // Erase the range [first, last).
    iterator erase( iterator f, iterator l) { return Base::erase(f,l); }



    // Remove the sub-range [cfirst,clast) from this list.
    void circular_remove( const Circulator& cfirst, const Circulator& clast) {
        if ( cfirst != 0) {
            assert( clast != 0);
            iterator i = cfirst.current_iterator();
            iterator j = clast.current_iterator();
            if ( i == j) {
                this->clear();
            } else {
                while ( i != j) {
                    iterator k = i++;
                    this->erase(k);
                    if ( i == this->end())
                        i = this->begin();
                }
            }
        }
    }
};


#endif // CIRCULAR_LIST_H
