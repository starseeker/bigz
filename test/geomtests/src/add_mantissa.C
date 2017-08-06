// Copyright 2003, 2004, 2006 Lutz Kettner, Max-Planck-Institute 
// fuer Informatik, Saarbruecken (Germany).
// Distributed under the Boost Software License, Version 1.0. (See accompany-
// ing file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//
// This file is part of 'Classroom Examples of Robustness Problems in Geometric
// Computations', <http://www.mpi-sb.mpg.de/~kettner/proj/NonRobust/>.
//
// add_mantissa.C
// Program to add an integer to the mantissa of a double, which allows
// the manipulation of double's in small exact increments.

#include <ieee_double.h>
#include <iostream>
#include <iomanip>

using namespace std;

int main( int argc, char* argv[]) {
    if ( argc != 3) {
        cerr << "Usage: add_mantissa <double> <int-mantissa>" << endl;
        return 1;
    }
    double d = atof( argv[1]);
    int    m = atoi( argv[2]);
    cout << setprecision(18) << ( d + Mantissa(m)) << endl;
    return 0;
}
