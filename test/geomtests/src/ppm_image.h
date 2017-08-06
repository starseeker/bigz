// Copyright 2003, 2004, 2006 Lutz Kettner, Max-Planck-Institute 
// fuer Informatik, Saarbruecken (Germany).
// Distributed under the Boost Software License, Version 1.0. (See accompany-
// ing file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//
// This file is part of 'Classroom Examples of Robustness Problems in Geometric
// Computations', <http://www.mpi-sb.mpg.de/~kettner/proj/NonRobust/>.
//
// ppm_image.h.h
// Class to represent, manipulate, load and save ppm (portable pixmap 
// file format) images supported by the tools of Jef Poskanzer and many
// viewers.
// Note: this is not a class that strives for completeness. It was developed
// for another project and is now used here for the very limited purpose
// of a having a convenient image output function.

#ifndef PPM_IMAGE_H
#define PPM_IMAGE_H

#include <cassert>
#include <iostream>
#include <vector>
#include <algorithm>
#include <string>

template <class T>
class Ppm_pixel;

// A RGB pixel proxy refers to a pixel value of three colors of type T.
template <class T>
class Ppm_image_pixel_proxy {
    T* m_ptr;
public:
    Ppm_image_pixel_proxy( T* ptr) : m_ptr(ptr) {}
    inline Ppm_image_pixel_proxy<T>& operator=(const Ppm_pixel<T>& px);
    T&       r()       { return m_ptr[0]; }
    T&       g()       { return m_ptr[1]; }
    T&       b()       { return m_ptr[2]; }
    const T  r() const { return m_ptr[0]; }
    const T  g() const { return m_ptr[1]; }
    const T  b() const { return m_ptr[2]; }
    T&       operator[](int i) {
	assert( i >= 0 && i < 3);
	return m_ptr[i];
    }
    const T  operator[](int i) const {
	assert( i >= 0 && i < 3);
	return m_ptr[i];
    }
};

// A RGB pixel stores three colors of type T.
template <class T>
class Ppm_pixel {
    T m_pix[3];
public:
    typedef Ppm_pixel<T> Self;

    Ppm_pixel() {} 
    Ppm_pixel( T r, T g, T b) { 
	m_pix[0] = r;
	m_pix[1] = g;
	m_pix[2] = b;
    }
    Ppm_pixel( const Ppm_image_pixel_proxy<T>& proxy) {
	m_pix[0] = proxy.r();
	m_pix[1] = proxy.g();
	m_pix[2] = proxy.b();
    }
    Ppm_pixel<T>& operator=( const Ppm_image_pixel_proxy<T>& proxy) {
	m_pix[0] = proxy.r();
	m_pix[1] = proxy.g();
	m_pix[2] = proxy.b();
	return *this;
    }
    bool operator==( const Self& pxl) { 
        return r() == pxl.r() &&  g() == pxl.g() &&  b() == pxl.b();
    }
    bool operator!=( const Self& pxl) { return ! (*this == pxl); }
    T&       r()       { return m_pix[0]; }
    T&       g()       { return m_pix[1]; }
    T&       b()       { return m_pix[2]; }
    const T  r() const { return m_pix[0]; }
    const T  g() const { return m_pix[1]; }
    const T  b() const { return m_pix[2]; }
    T&       operator[](int i) {
	assert( i >= 0 && i < 3);
	return m_pix[i];
    }
    const T  operator[](int i) const {
	assert( i >= 0 && i < 3);
	return m_pix[i];
    }
};

template <class T> inline
Ppm_image_pixel_proxy<T>& 
Ppm_image_pixel_proxy<T>::operator=( const Ppm_pixel<T>& pix) {
    m_ptr[0] = pix.r();
    m_ptr[1] = pix.g();
    m_ptr[2] = pix.b();
    return *this;
}

// A row proxy represents a one dimensional row of a 2d image.
// Accessing an element returns a pixel proxy.
template <class T>
class Ppm_image_row_proxy {
    T* m_ptr;
public:
    Ppm_image_row_proxy( T* ptr) : m_ptr(ptr) {}
    Ppm_image_pixel_proxy<T>  operator[]( unsigned int x ) {
	return Ppm_image_pixel_proxy<T>( m_ptr + 3 * x);
    }
    const Ppm_image_pixel_proxy<T>  operator[]( unsigned int x ) const {
	return Ppm_image_pixel_proxy<T>((T*)( m_ptr + 3 * x));
    }
};

// 2D RGB image with 2d array access (implemented via proxy classes)
// Type T is used for storing a color. A pixel stores three color values.
template <class T>
class Ppm_image {
public:
    typedef std::vector<T>                      Container;
    typedef typename Container::iterator        iterator;
    typedef typename Container::const_iterator  const_iterator;
    typedef typename Container::size_type       size_type;
    typedef typename Container::difference_type difference_type;
    typedef Ppm_pixel<T>                    Pixel;

private:
    size_type        m_width;
    size_type        m_height;
    T                m_max;
    bool             m_binary;
    std::vector<T>   m_pix; // 1d-image data, 2d conversion done in this class

public:
    Ppm_image() : m_width(0), m_height(0), m_max(0), m_binary(false) {}
    Ppm_image( size_type width, size_type height, T max = 255, T val = 0) 
	: m_width(width), m_height(height), m_max(max), m_binary(false), 
	  m_pix( 3 * width * height, val) {}
    Ppm_image( size_type width, size_type height, const Pixel& pix, T max =255)
	: m_width(width), m_height(height), m_max(max), m_binary(false), 
	  m_pix( 3 * width * height)        
    {
        for ( iterator i = m_pix.begin(); i != m_pix.end(); ) {
            *i++ = pix.r();
            *i++ = pix.g();
            *i++ = pix.b();
        }
    }

    void swap( Ppm_image<T>& image) {
	std::swap( m_width,          image.m_width);
	std::swap( m_height,         image.m_height);
	std::swap( m_max,            image.m_max);
	std::swap( m_binary,         image.m_binary);
	m_pix.swap( image.m_pix);
    }

    size_type width()  const { return m_width; }
    size_type height() const { return m_height; }
    int       max()    const { return m_max; }
    void      set_max( T m)  { m_max = m; }
    T         compute_max() {
                  m_max = *(std::max_element( begin(), end()));
                  return m_max;
    }
    void      clip_at_max( T max) {
                  m_max = max;
                  for ( iterator i = begin(); i != end(); ++i)
                      if ( *i > max)
                          *i = max;
    }


    bool      binary_io() const         { return m_binary; }
    void      set_binary_io( bool bin)  { m_binary = bin; }

    iterator       begin()       { return m_pix.begin(); }
    const_iterator begin() const { return m_pix.begin(); }
    iterator       end()         { return m_pix.end(); }
    const_iterator end()   const { return m_pix.end(); }

    Ppm_image_row_proxy<T>        operator[]( size_type y ) {
	return Ppm_image_row_proxy<T>(&*(begin() + 3 * y * m_width));
    }
    const Ppm_image_row_proxy<T>  operator[]( size_type y ) const {
	return Ppm_image_row_proxy<T>((T*)(&*(begin() + 3 * y * m_width)));
    }

    // Skip ppm ASCII comment lines starting with '#' (IO support function)
    static void skip_comment( std::istream& in) {
        char c = ' ';
        in >> c;
        while ( in && c == '#' ) {
            while ( in.get(c) && c != '\n')
                ;
            in >> c;
        }
        if (in) 
            in.putback(c);
    }
};


// Stream input, reads ppm format in binary as well as in ASCII format, 
// sets the binary information bit in image accordingly
template < class T >
std::istream& operator>>( std::istream& in, Ppm_image<T>& image) {
    typedef typename Ppm_image<T>::size_type size_type;
    image.skip_comment( in);
    char c, d;
    in >> c >> d;
    if ( c != 'P' || (d != '3' && d != '6')) {
	in.clear( std::ios::badbit);
	std::cerr << "Reading Ppm_image: wrong magic number, not a ppm "
	             "P3/P6 type." << std::endl;
	return in;
    }
    image.skip_comment( in);
    size_type width, height;
    unsigned int max_val;
    in >> width;
    image.skip_comment( in);
    in >> height;
    image.skip_comment( in);
    in >> max_val;
    if ( in) {
	image = Ppm_image<T>(); // erase old, than allocate new
	Ppm_image<T> dummy( width, height, max_val);
	image.swap( dummy);
	image.set_binary_io( d == '6');
	if ( d == '6') {
	    in.get(c); // get rid of last '\n' before the binary data
	    for (size_type y = 0; y < height; ++y) {
		for (size_type x = 0; x < width; ++x) {
		    char pixel;
		    in.get( pixel);
		    image[y][x].r() = T((unsigned char)(pixel));
		    in.get( pixel);
		    image[y][x].g() = T((unsigned char)(pixel));
		    in.get( pixel);
		    image[y][x].b() = T((unsigned char)(pixel));
		}
	    }
	} else {
	    for (size_type y = 0; y < height; ++y) {
		for (size_type x = 0; x < width; ++x) {
		    image.skip_comment( in);
		    unsigned int pixel;
		    in >> pixel;
		    image[y][x].r() = T(pixel);
		    in >> pixel;
		    image[y][x].g() = T(pixel);
		    in >> pixel;
		    image[y][x].b() = T(pixel);
		}
	    }
	}
	if ( ! in) {
	    std::cerr << "Reading Ppm_image: error reading pixel information."
                      << std::endl;
	}
    } else {
	std::cerr << "Reading Ppm_image: error reading header information."
		  << std::endl;
    }
    return in;
}

// Stream output, either in binary or ASCII ppm format, depending in
// the binary information bit in image
template < class T >
std::ostream& operator<<( std::ostream& out, const Ppm_image<T>& image) { 
    typedef typename Ppm_image<T>::size_type size_type;
    bool binary = image.binary_io();
    if ( binary && int(image.max()) > 255) {
	std::cerr << "Writing Ppm_image: max > 255 cannot be written "
	             "binary, writing ASCII" << std::endl;
	binary = false;
    }
    out << (binary ? "P6\n" : "P3\n");
    out << image.width() << ' ' << image.height()
	<< "  # width x height\n" << (unsigned int)(image.max())
	<< (binary ? "" : "  # max color component value") << std::endl;
    if ( binary ) {
	for ( size_type y = 0; y < image.height(); ++y) {
	    for ( size_type x = 0; x < image.width(); ++x) {
		out << (unsigned char)( image[y][x].r())
		    << (unsigned char)( image[y][x].g())
		    << (unsigned char)( image[y][x].b());
	    }
	}
    } else {
	for ( size_type y = 0; y < image.height(); ++y) {
	    for ( size_type x = 0; x < image.width(); ++x) {
		out << (unsigned int)(image[y][x].r()) << ' '
		    << (unsigned int)(image[y][x].g()) << ' '
		    << (unsigned int)(image[y][x].b()) 
		    << (( x % 4 == 3) ? '\n' : ' ');
	    }
	    out << '\n';
	}
	out << std::endl;
    }
    return out;
}


#endif // PPM_IMAGE_H

