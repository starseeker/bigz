# Copyright 2003, 2004, 2006 Lutz Kettner, Max-Planck-Institute 
# fuer Informatik, Saarbruecken (Germany).
# Distributed under the Boost Software License, Version 1.0. (See accompany-
# ing file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
#
# This file is part of 'Classroom Examples of Robustness Problems in Geometric
# Computations', <http://www.mpi-sb.mpg.de/~kettner/proj/NonRobust/>.
#
# Makefile
# Important targets: all, depend, clean, and all in $(TARGETS)

CXX      = g++
CXXFLAGS = -I. -Wall -g # don't use optimizations, they may change 
                        # the intended IEEE double behavior

TARGETS =       add_mantissa 			\
		points2bin			\
		bin2points			\
                incr_convex_hull 		\
                gift_wrapping_hull 		\
		fp_scope			\
		fp_scope_off			\
                ieee_double

DEPENDSRC = 	add_mantissa.C 			\
		points2bin.C			\
		bin2points.C			\
                incr_convex_hull.C 		\
                gift_wrapping_hull.C	 	\
		fp_scope.C			\
		fp_scope_off.C			\
                ieee_double.C


all: $(TARGETS)

include Makefile_depend

depend: $(DEPENDSRC)
	$(CXX) -MM $(CXXFLAGS) $(DEPENDSRC) | sed 's/[^ ]*[/]CGAL[/]include[^ ]* *//g'  | sed 's/[^ ]*[/]LEDA[/]incl[^ ]* *//g'  | sed 's/[^ ]*[/]gmp[/]include[^ ]* *//g' | grep -v '^ *[\\] *$$' > Makefile_depend


add_mantissa: add_mantissa.o
	$(CXX) -o add_mantissa add_mantissa.o

points2bin: points2bin.o
	$(CXX) -o points2bin points2bin.o

bin2points: bin2points.o
	$(CXX) -o bin2points bin2points.o

incr_convex_hull: incr_convex_hull.o
	$(CXX) -o incr_convex_hull incr_convex_hull.o

gift_wrapping_hull: gift_wrapping_hull.o
	$(CXX) -o gift_wrapping_hull gift_wrapping_hull.o

fp_scope: fp_scope.o
	$(CXX) -o fp_scope fp_scope.o

fp_scope_off: fp_scope_off.o
	$(CXX) -o fp_scope_off fp_scope_off.o

ieee_double: ieee_double.o
	$(CXX) -o ieee_double ieee_double.o

clean:
	rm -f *.o $(TARGETS)



#---------------------------------------------------------------------#
#                    suffix rules
#---------------------------------------------------------------------#

.C.o:
	$(CXX) $(CXXFLAGS) -c $<

