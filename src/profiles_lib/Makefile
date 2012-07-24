CC = g++
CPPFLAGS = -I.
CXXFLAGS = -O3 -ansi -pedantic -W -Wall -Wextra -std=c++0x

sources = m_delta.cc m_delta_builder.cc m_profile.cc m_fileutils.cc maf_read_stream.cc
objects = $(sources:%.cc=%.o)

.PHONY: all native-code byte-code debug-code clean

all: libprofile.a

native-code: all

byte-code: all

debug-code: all

libprofile.a: $(objects)
	ar -r libprofile.a $(objects)

m_delta.cc: m_delta.hh

m_profile.cc: m_profile.hh m_option.hh m_range.hh

m_fileutils.cc: m_fileutils.hh

maf_read_stream.cc: maf_read_stream.hh m_option.hh m_direction.hh

clean:
	-rm $(objects) libprofile.a
