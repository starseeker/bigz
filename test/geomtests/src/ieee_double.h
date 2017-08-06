// Copyright 2003, 2004, 2006 Lutz Kettner, Max-Planck-Institute 
// fuer Informatik, Saarbruecken (Germany).
// Distributed under the Boost Software License, Version 1.0. (See accompany-
// ing file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//
// This file is part of 'Classroom Examples of Robustness Problems in Geometric
// Computations', <http://www.mpi-sb.mpg.de/~kettner/proj/NonRobust/>.
//
// ieee_double.h
// Helper functions to operate on IEEE double precision float numbers.
// Note: These are small helper functions sufficient to cover the part
// of the IEEE Std 754-1985 floating-point standard needed for our 
// experiments, not general purpose functions.

#ifndef IEEE_DOUBLE_H
#define IEEE_DOUBLE_H

#include <string>
#include <cassert>
#include <iostream>
#include <iomanip>
#include <sstream>

// Typedef to some unsigned integral type of (at least) 64 bit length.
typedef unsigned long long ullong;

// Returns the mantissa of a double d in an integral type.
inline ullong mantissa( double d) {
    ullong i = * reinterpret_cast<ullong*>(&d);
    return i & 0xfffffffffffffULL;
}

// Returns the double d, of which the mantissa has been replaced by the
// given integral type.
inline double set_mantissa( double d, ullong mantissa) {
    ullong& i = * reinterpret_cast<ullong*>(&d);
    i = (i & 0xfff0000000000000ULL) | mantissa;
    return d;
}

// Returns the exponent, in the biased notation, of a double d, in an 
// integral type.
inline ullong biased_exponent( double d) {
    ullong i = * reinterpret_cast<ullong*>(&d);
    return (i & 0x7ff0000000000000ULL) >> 52;
}

// Returns the double d, of which the exponent has been replaced by the
// given integral type in biased exponent notation.
inline double set_biased_exponent( double d, ullong exponent) {
    ullong& i = * reinterpret_cast<ullong*>(&d);
    i = (i & 0x800fffffffffffffULL) | ( exponent << 52);
    return d;
}

// Returns the exponent, in the un-biased notation, of a double d, in an 
// integral type.
inline int exponent( double d) { return int( biased_exponent(d)) - 1023; }

// Returns the double d, of which the exponent has been replaced by the
// given integral typ in un-biased exponent notation.
inline double set_exponent( double d, int exponent) {
    return set_biased_exponent( d, ullong( exponent + 1023));
}

// Returns true if a double d is negative (queries sign bit).
inline bool dsign( double d) {
    ullong i = * reinterpret_cast<ullong*>(&d);
    bool s = ((i & 0x8000000000000000ULL) != 0ULL);
    assert( s ? (d<0.0) : (d>=0.0));
    return s;
}

// Returns the double d where the sign bit has been set to sign.
inline double set_dsign( double d, bool sign) {
    ullong& i = * reinterpret_cast<ullong*>(&d);
    if ( sign)
        i = (i | 0x8000000000000000ULL);
    else
        i = (i & 0x7fffffffffffffffULL);
    return d;
}

// Increments the double value d by the smallest possible value, i.e.,
// the smallest bit in the mantissa. Returns d.
inline double& incr( double& d) {
    if ( ! dsign(d)) {
        ullong m = mantissa(d);
        ++m;
        m = m & 0xfffffffffffffULL;
        if ( m == 0ULL) // overflow
            d = set_exponent( d, exponent(d) + 1);
        d = set_mantissa(d,m);
    } else {
        ullong m = mantissa(d);
        if ( m == 0ULL) // underflow
            d = set_exponent( d, exponent(d) - 1);
        --m;
        m = m & 0xfffffffffffffULL;
        d = set_mantissa(d,m);
    }
    return d;
}

// Adds i to the mantissa of the double value d. Here, the mantissa is viewed
// as an integer value to which we can add the integer i (also respecting
// potential overflow and change of the exponent). Returns d.
inline double& add_to_mantissa( double& d, int i) {
    if ( ! dsign(d)) { // d is positive
        if ( i >= 0) {
            ullong m = mantissa(d);
            ullong mm = (m + ullong(i) & 0xfffffffffffffULL);
            if ( mm < m) // overflow, since i is 32 bit, only one incr in e
                d = set_exponent( d, exponent(d) + 1);
            d = set_mantissa(d,mm);
        } else {
            ullong m = mantissa(d);
            ullong mm = (m - ullong(-i) & 0xfffffffffffffULL);
            if ( mm > m) // underflow, since i is 32 bit, only one decr in e
                d = set_exponent( d, exponent(d) - 1);
            d = set_mantissa(d,mm);
        }
    } else {
        if ( i >= 0) {
            ullong m = mantissa(d);
            ullong mm = (m - ullong(i) & 0xfffffffffffffULL);
            if ( mm > m) // underflow, since i is 32 bit, only one incr in e
                d = set_exponent( d, exponent(d) - 1);
            d = set_mantissa(d,mm);
        } else {
            ullong m = mantissa(d);
            ullong mm = (m + ullong(-i) & 0xfffffffffffffULL);
            if ( mm < m) // overflow, since i is 32 bit, only one decr in e
                d = set_exponent( d, exponent(d) + 1);
            d = set_mantissa(d,mm);
        }
    }
    return d;
}

// Helper type to have nice operator notations that can add integers
// to double's mantissa. (An add operator adding doubles and ints would 
// be ambiguous or, at least, really confusing to users ;-)
struct Mantissa {
    int a;
    Mantissa() : a(0) {}
    Mantissa( int i) : a(i) {}
};

// Adds i to the mantissa of the double value d. Here, the mantissa is viewed
// as an integer value to which we can add the integer i (also respecting
// potential overflow and change of the exponent). Returns d.
inline double operator+( double d, Mantissa m) {
    add_to_mantissa( d, m.a);
    return d;
}
    
// Adds i to the mantissa of the double value d. Here, the mantissa is viewed
// as an integer value to which we can add the integer i (also respecting
// potential overflow and change of the exponent). Returns d.
inline double operator+( Mantissa m, double d) {
    add_to_mantissa( d, m.a);
    return d;
}

// Decrements the double value d by the smallest possible value, i.e.,
// the smallest bit in the mantissa. Returns d.
inline double& decr( double& d) {
    if ( ! dsign(d)) {
        ullong m = mantissa(d);
        if ( m == 0ULL) // underflow
            d = set_exponent( d, exponent(d) - 1);
        --m;
        m = m & 0xfffffffffffffULL;
        d = set_mantissa(d,m);
    } else {
        ullong m = mantissa(d);
        ++m;
        m = m & 0xfffffffffffffULL;
        if ( m == 0ULL) // overflow
            d = set_exponent( d, exponent(d) + 1);
        d = set_mantissa(d,m);
    }
    return d;
}
    

// Returns a string with a binary representation of n_bits of a 
// unsigned long long.
inline std::string bits_from_ullong( ullong i, int n_bits, 
                                     bool spacing = true) {
    std::string s;
    int k = 1;
    while ( k <= n_bits) {
        s.insert( s.begin(), ((i&1) ? '1' : '0'));
        if ( (k++ % 4) == 0 && k < n_bits && spacing)
            s.insert( s.begin(), ' ');
        i = i >> 1;
    }
    return s;
}

// Conversion table from index to hexadecimal characters.
const char* hex_digits = "0123456789abcdef";

// Returns a string with a hex representation of n_bits of a 
// unsigned long long.
inline std::string hex_from_ullong( ullong i, int n_bits) {
    std::string s;
    int k = 1;
    while ( k <= n_bits) {
        s.insert( s.begin(), hex_digits[ i & 0xf]);
        i = i >> 4;
        k += 4;
    }
    return s;
}

// Returns a string with a binary representation of the 64 bits of a double.
inline std::string bits_from_double( double d) {
    ullong i = * reinterpret_cast<ullong*>(&d);
    return bits_from_ullong( i, 64);
}

// Returns a string with a scientific binary representation of a double.
inline std::string bin( double d) {
    std::string s( ! dsign(d) ? "+" : "-");
    ullong ex = biased_exponent(d);
    ullong m  = mantissa(d);
    if ( ex == 0ULL && m != 0ULL)
        std::cerr << "WARNING: denormalized numbers!" << std::endl;
    if ( ex == 2047ULL)
        std::cerr << "WARNING: NaN or +-Inf numbers!" << std::endl;
    if ( ex == 0ULL)
        s = s + "0.";
    else
        s = s + "1.";
    s = s + bits_from_ullong( m, 52) + "_E_" + bits_from_ullong( ex, 11);
    return s;
}

// Returns a string with a scientific hex representation of a double.
inline std::string hex( double d) {
    std::string s( ! dsign(d) ? "+" : "-");
    ullong ex = biased_exponent(d);
    ullong m  = mantissa(d);
    if ( ex == 0ULL && m != 0ULL)
        std::cerr << "WARNING: denormalized numbers!" << std::endl;
    if ( ex == 2047ULL)
        std::cerr << "WARNING: NaN or +-Inf numbers!" << std::endl;
    if ( ex == 0ULL)
        s = s + "0.";
    else
        s = s + "1.";
    s = s + hex_from_ullong( m, 52) + "_E_" + hex_from_ullong( ex, 11);
    return s;
}

// Returns true if a double value remains unchanged by iostream conversions. 
bool exact_io_conversion( const double& d) {
    std::stringstream ss;
    ss << std::setprecision(18) << d; 
    double ld;
    ss >> ld;
    return ( ld == d);
}

#endif // IEEE_DOUBLE_H
