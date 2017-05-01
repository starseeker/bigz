// Copyright 2003, 2004, 2006 Lutz Kettner, Max-Planck-Institute 
// fuer Informatik, Saarbruecken (Germany).
// Distributed under the Boost Software License, Version 1.0. (See accompany-
// ing file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//
// This file is part of 'Classroom Examples of Robustness Problems in Geometric
// Computations', <http://www.mpi-sb.mpg.de/~kettner/proj/NonRobust/>.
//
// ieee_double.C
// test for helper functions to operate on IEEE double precision float numbers.
// Note: These are small helper functions sufficient to cover the part
// of the IEEE Std 754-1985 floating-point standard needed for our 
// experiments, not general purpose functions.

#include <ieee_double.h>
#include <iostream>

using namespace std;

int main() {
    {
        double d = 0.5;
        cout << hex(d) << "  " << bin(d) << endl;
        incr(d); assert( d > 0.5);
        cout << hex(d) << "  " << bin(d) << endl;
        incr(d); assert( d > 0.5);
        cout << hex(d) << "  " << bin(d) << endl;
        decr(d); assert( d > 0.5);
        decr(d); assert( d == 0.5);
        cout << hex(d) << "  " << bin(d) << endl;
        decr(d); assert( d < 0.5);
        cout << hex(d) << "  " << bin(d) << endl;
        incr(d); assert( d == 0.5);
        cout << hex(d) << "  " << bin(d) << endl;
    }{
        double d = -0.5;
        cout << hex(d) << "  " << bin(d) << endl;
        incr(d); assert( d > -0.5);
        cout << hex(d) << "  " << bin(d) << endl;
        incr(d); assert( d > -0.5);
        cout << hex(d) << "  " << bin(d) << endl;
        decr(d); assert( d > -0.5);
        decr(d); assert( d == -0.5);
        cout << hex(d) << "  " << bin(d) << endl;
        decr(d); assert( d < -0.5);
        cout << hex(d) << "  " << bin(d) << endl;
        incr(d); assert( d == -0.5);
        cout << hex(d) << "  " << bin(d) << endl;

    }{
        double d = 0.5;
        cout << hex(d) << "  " << bin(d) << endl;
        add_to_mantissa(d,256); assert( d > 0.5);
        cout << hex(d) << "  " << bin(d) << endl;
        add_to_mantissa(d,256); assert( d > 0.5);
        cout << hex(d) << "  " << bin(d) << endl;
        add_to_mantissa(d,-512); assert( d == 0.5);
        cout << hex(d) << "  " << bin(d) << endl;
        add_to_mantissa(d,-256); assert( d < 0.5);
        cout << hex(d) << "  " << bin(d) << endl;
        add_to_mantissa(d,256); assert( d == 0.5);
        cout << hex(d) << "  " << bin(d) << endl;
    }{
        double d = -0.5;
        cout << hex(d) << "  " << bin(d) << endl;
        add_to_mantissa(d,256); assert( d > -0.5);
        cout << hex(d) << "  " << bin(d) << endl;
        add_to_mantissa(d,256); assert( d > -0.5);
        cout << hex(d) << "  " << bin(d) << endl;
        add_to_mantissa(d,-512); assert( d == -0.5);
        cout << hex(d) << "  " << bin(d) << endl;
        add_to_mantissa(d,-256); assert( d < -0.5);
        cout << hex(d) << "  " << bin(d) << endl;
        add_to_mantissa(d,256); assert( d == -0.5);
        cout << hex(d) << "  " << bin(d) << endl;
    }{
        double d = 0.5;
        cout << hex(d) << "  " << bin(d) << endl;
        d = d + Mantissa(256); assert( d > 0.5);
        cout << hex(d) << "  " << bin(d) << endl;
        d = Mantissa(256) + d; assert( d > 0.5);
        cout << hex(d) << "  " << bin(d) << endl;
        d = d + Mantissa(-512); assert( d == 0.5);
        cout << hex(d) << "  " << bin(d) << endl;
        d = Mantissa(-256) + d; assert( d < 0.5);
        cout << hex(d) << "  " << bin(d) << endl;
        d = d + Mantissa(256); assert( d == 0.5);
        cout << hex(d) << "  " << bin(d) << endl;
    }
    return 0;
}
