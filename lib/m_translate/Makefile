CC = g++
CPPFLAGS = -I. -I../profiles_lib
CXXFLAGS = -O3 -ansi -pedantic -W -Wall -Wextra -std=c++0x
LDFLAGS  = -L../profiles_lib -lprofiles

sources = m_translate.cc m_translate_main.cc
objects = $(sources:%.cc=%.o)

.PHONY: all test native-code byte-code clean

vpath %.hh ../profiles_lib

all: m_translate

test:

native-code: all

byte-code:

m_translate: $(objects)
	$(CC) -o $@ $(objects) $(LDFLAGS)

m_translate.cc: m_delta.hh m_delta_builder.hh \
	m_profile.hh m_metaprofile.hh \
	m_delta_stream_writer.hh m_translate.hh \
	m_fileutils.hh

m_translate_main.cc: m_translate.o

clean:
	-rm m_translate $(objects)

