OUTPUT = m_sort_delta m_translate maf_analyzer

CC = g++
CPPFLAGS = -I.
CXXFLAGS = -O3 -ansi -pedantic -W -Wall -Wextra -std=c++0x

.PHONY : all native-code byte-code debug-code clean
all : $(OUTPUT)

native-code: all

byte-code: all

debug-code: all

m_sort_delta : m_delta.o m_sort_delta.o

m_translate : m_delta.o m_delta_builder.o m_profile.o m_fileutils.o m_translate.o m_translate_main.o

maf_analyzer : maf_analyzer.o maf_read_stream.o maf_analyzer_missing.o

m_sort_delta.o : m_sort_delta.cc

m_sort_delta.cc : m_option.hh m_delta.hh m_delta_stream_writer.hh

m_translate.o : m_translate.cc

m_translate.cc : m_delta.hh m_delta_builder.hh m_profile.hh m_metaprofile.hh m_delta_stream_writer.hh m_translate.hh m_fileutils.hh

m_translate_main.o : m_translate_main.cc

m_translate_main.cc : m_fileutils.hh m_translate.hh

maf_analyzer.o : maf_analyzer.cc

maf_analyzer.cc : maf_read_stream.hh

maf_analyzer_missing.cc : maf_analyzer_missing.hh maf_read_stream.hh

maf_analyzer_missing.hh : maf_read_stream.hh

clean :
	-rm $(OUTPUT) *.o
