CC = g++
CPPFLAGS = -I.
CXXFLAGS = -O3 -ansi -pedantic -W -Wall -Wextra -std=c++0x

sources = m_delta.cc m_delta_builder.cc m_profile.cc m_fileutils.cc maf_read_stream.cc
objects = $(sources:%.cc=%.o)

profiles_lib = libprofiles.a

.PHONY: all test native-code clean

all: $(profiles_lib)

native-code: all

test:

$(profiles_lib): $(objects)
	ar rvs $(profiles_lib) $(objects)

m_delta.cc: m_delta.hh

m_profile.cc: m_profile.hh m_option.hh m_range.hh

m_fileutils.cc: m_fileutils.hh

maf_read_stream.cc: maf_read_stream.hh m_option.hh m_direction.hh

clean:
	-rm $(objects) $(profiles_lib)
