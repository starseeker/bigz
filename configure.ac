#
# Autoconf script for OpenLisp.
# By Christian Jullien (jullien@eligis.com)
# $Id: configure.ac,v 1.2 2016/05/05 07:44:16 jullien Exp $
#
# use autoconf-2.68 (works also with autoconf-2.13)

# Process this file with autoconf to produce a configure script

AC_INIT(bign.h)

# where to find install.sh, config.sub, and config.guess

AC_CONFIG_AUX_DIR(./conf)

# use ./config.h.in template.

AC_CONFIG_HEADER(./config.h)

# make sure we are using a recent autoconf version

AC_PREREQ(2.63)

NAME=bignum
VERSION=`grep VERSION bigz.h | sed -e "s/.*\"\(.*\)\"/\1/g"`

# User options

AC_CANONICAL_SYSTEM

# Checks for C compiler

AC_PROG_CC

# Checks for specific system

AC_AIX

# Checks for C++ (Must be called after AC_AIX)

AC_MSG_CHECKING(for C++)
AC_ARG_ENABLE(cxx,
[  --enable-cxx            enable C++ lib [default=yes]],,enable_cxx=y)
if test "$enable_cxx" = "y" || test "$enable_cxx" = "yes"
then
   enable_cxx=yes
   NOCXX=""
   AC_MSG_RESULT(yes)
   AC_PROG_CXX
   AC_LANG(C++)
   AX_CHECK_COMPILE_FLAG([-std=c++14], [
                           CXXFLAGS="$CXXFLAGS -std=c++14"])
   if test x"$ax_cv_check_cxxflags___std_cpp14" = x"no"
   then
     AX_CHECK_COMPILE_FLAG([-std=c++11], [
                             CXXFLAGS="$CXXFLAGS -std=c++11"])
   fi
   AC_LANG(C)
else
   NOCXX="# "
   AC_MSG_RESULT(no)
fi

AC_MSG_CHECKING(for PGO - Profile Guided Optimization)
AC_ARG_ENABLE(pgo,
[  --enable-pgo            enable PGO optimization [default=no]],,enable_pgo=nol)
if test "$enable_pgo" = "y" || test "$enable_pgo" = "yes"
then
   enable_pgo=yes
   STATIC_BUILD=pgo
   AC_MSG_RESULT(yes)
else
   STATIC_BUILD=std
   AC_MSG_RESULT(no)
fi
AC_SUBST(STATIC_BUILD)

AC_CHECK_PROGS(MAKE, gmake, make)
if test $MAKE = no
then
  AC_MSG_ERROR(Can\'t find gmake or make.)
fi

AC_CHECK_PROGS(AR, ar gar, no)
if test $AR = no; then
  AC_MSG_ERROR(Can\'t find ar or gar.)
fi
AC_SUBST(AR)

AC_CHECK_PROGS(RANLIB, ranlib, no)
if test $RANLIB = no; then
  RANLIB="echo INFO: no ranlib on this system "
fi
AC_SUBST(RANLIB)

AC_CHECK_PROGS(LDCONFIG, ldconfig, no)
if test $LDCONFIG = no; then
  LDCONFIG="echo INFO: no ldconfig on this system "
fi
AC_SUBST(LDCONFIG)

# testkern can mesure time using clock_gettime which is generally in librt.

AC_CHECK_FUNC([clock_gettime],,   [AC_CHECK_LIB(rt,     clock_gettime)])

# Checks for header files.

AC_HEADER_STDC

# Checks for typedefs, structures, and compiler characteristics.

AC_C_CONST
AC_C_RESTRICT
AC_C_INLINE

AC_TYPE_SIZE_T

if test "$ac_cv_c_inline" != no
then
  AC_DEFINE(HAVE_INLINE)
  AC_SUBST(HAVE_INLINE)
  AC_SUBST(_INLINE_KEYWORD)
  AC_DEFINE_UNQUOTED(_INLINE_KEYWORD, $ac_cv_c_inline)
else
  AC_SUBST(_INLINE_KEYWORD)
  AC_DEFINE_UNQUOTED(_INLINE_KEYWORD, /* no inline */)
fi

if test "$ac_cv_c_restrict" != no
then
  AC_DEFINE(HAVE_RESTRICT)
  AC_SUBST(HAVE_RESTRICT)
  AC_SUBST(_RESTRICT_KEYWORD)
  AC_DEFINE_UNQUOTED(_RESTRICT_KEYWORD, $ac_cv_c_restrict)
else
  AC_SUBST(_RESTRICT_KEYWORD)
  AC_DEFINE_UNQUOTED(_RESTRICT_KEYWORD, /* no restrict */)
fi

# get type size

AC_CHECK_SIZEOF(void *, 4)
AC_CHECK_SIZEOF(int,    4)
AC_CHECK_SIZEOF(long,   4)
AC_CHECK_SIZEOF(size_t, 4)

# Save cache

AC_CACHE_SAVE

AC_PROG_GCC_TRADITIONAL

# OS specifics, sets {EXE,OBJ}EXT

AC_EXEEXT
AC_OBJEXT

dnl Optimization and Warnings options

AX_CHECK_COMPILE_FLAG([-m64],
                      [CFLAGS="$CFLAGS -m64"])
AX_CHECK_COMPILE_FLAG([-O3],
                      [CFLAGS="$CFLAGS -O3"])
AX_CHECK_COMPILE_FLAG([-Wall],
                      [CFLAGS="$CFLAGS -Wall"])
AX_CHECK_COMPILE_FLAG([-Wextra],
                      [CFLAGS="$CFLAGS -Wextra"])
AX_CHECK_COMPILE_FLAG([-Wshadow],
                      [CFLAGS="$CFLAGS -Wshadow"])
AX_CHECK_COMPILE_FLAG([-Wunused],
                      [CFLAGS="$CFLAGS -Wunused"])
AX_CHECK_COMPILE_FLAG([-Wunused-value],
                      [CFLAGS="$CFLAGS -Wunused-value"])
AX_CHECK_COMPILE_FLAG([-Wunused-variable],
                      [CFLAGS="$CFLAGS -Wunused-variable"])
AX_CHECK_COMPILE_FLAG([-Wunused-parameter],
                      [CFLAGS="$CFLAGS -Wunused-parameter"])
AX_CHECK_COMPILE_FLAG([-Wunused-label],
                      [CFLAGS="$CFLAGS -Wunused-label"])
AX_CHECK_COMPILE_FLAG([-Wunused-function],
                      [CFLAGS="$CFLAGS -Wunused-function"])
AX_CHECK_COMPILE_FLAG([-fomit-frame-pointer],
                      [CFLAGS="$CFLAGS -fomit-frame-pointer"])
AX_CHECK_COMPILE_FLAG([-fprofile-generate],
                      [PGO_GEN="-fprofile-generate"])
AX_CHECK_COMPILE_FLAG([-fprofile-use],
                      [PGO_USE="-fprofile-use"])

if test `uname` != Darwin
then
  AX_CHECK_COMPILE_FLAG([-finline-functions],
                        [CFLAGS="$CFLAGS -finline-functions"])
  AX_CHECK_COMPILE_FLAG([-finline-limit=4096],
                        [CFLAGS="$CFLAGS -finline-limit=4096"])
fi

# Local options

AEXT="a"
AROPTS="-c -r"
PIC=""
SHARED="shared-std"
SOEXT="so"

if test `uname` != Darwin
then
  SONAME=soname
else
  SONAME=install_name
fi

case "${CC}" in
  *tcc*)
        # No C++ with tcc
        NOCXX="# "
	SHARED=""
        ;;
  icc)
	#OPTIMS="-g -O3 -mcpu=pentiumpro -axi -ip -unroll -I."
	;;
esac

# Option "-mcpu=`uname -m`"

AC_SUBST(NAME)
AC_SUBST(AEXT)
AC_SUBST(AROPTS)
AC_SUBST(CFLAGS)
AC_SUBST(LIBS)
AC_SUBST(CXX)
AC_SUBST(NOCXX)
AC_SUBST(LDFLAGS)
AC_SUBST(PIC)
AC_SUBST(SHARED)
AC_SUBST(SOEXT)
AC_SUBST(SONAME)
AC_SUBST(VERSION)
AC_SUBST(PGO_GEN)
AC_SUBST(PGO_USE)

# Save result in Makefile (using Makefile.in template).

AC_OUTPUT(Makefile ${NAME}.spec ${NAME}.pc)

echo \
"------------------------------------------------------------------------
Configuration:

 Name:		${NAME}
 Version:	${VERSION}
 Source:        ${srcdir}
 Compiler:	${CC} ${CCVERSION}
 Flags:         ${CFLAGS}
 Build:         ${build}
 Host:          ${host}
 Target:        ${target}
 Install:       ${prefix}

See ./config.h for further configuration information.
------------------------------------------------------------------------"
