#!/bin/sh
#
# Copyright (c) 1992-2015, Eligis
#

fail() {
    echo "Error:" $@ 1>&2; exit 1
}

aclocal -I m4 --force || fail "aclocal"
autoconf -f || fail "autoconf"
