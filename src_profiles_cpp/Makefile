OUTPUT = m_sort_delta m_translate maf_analyzer

CC = g++
CPPFLAGS = -I.
CXXFLAGS = -O3 -ansi -pedantic -W -Wall -Wextra

.PHONEY : all
all : $(OUTPUT)

m_sort_delta : m_delta.o m_sort_delta.o

m_translate : m_delta.o m_delta_builder.o m_profile.o m_fileutils.o m_translate.o m_translate_main.o

maf_analyzer : maf_analyzer.o maf_read_stream.o maf_analyzer_missing.o

m_delta.o : m_delta.cc

m_sort_delta.o : m_sort_delta.cc

m_delta.cc : m_delta.hh

m_delta.hh : m_option.hh m_range.hh m_profile.hh

m_range.hh : m_option.hh m_direction.hh

m_profile.hh : m_option.hh m_range.hh

m_sort_delta.o : m_sort_delta.cc

m_sort_delta.cc : m_option.hh m_delta.hh m_delta_stream_writer.hh

m_delta_builder.o : m_delta_builder.cc

m_delta_builder.cc : m_option.hh m_delta.hh m_metaprofile.hh m_delta_builder.hh

m_metaprofile.hh : m_profile.hh

m_delta_builder.hh : m_delta.hh m_metaprofile.hh

m_fileutils.o : m_fileutils.cc

m_fileutils.cc: m_fileutils.hh

m_translate.o : m_translate.cc

m_translate.cc : m_delta.hh m_delta_builder.hh m_profile.hh m_metaprofile.hh m_delta_stream_writer.hh m_translate.hh m_fileutils.hh

m_translate_main.o : m_translate_main.cc

m_translate_main.cc : m_fileutils.hh m_translate.hh

maf_analyzer.o : maf_analyzer.cc

maf_analyzer.cc : maf_read_stream.hh

maf_read_stream.o : maf_read_stream.cc

maf_read_stream.cc : maf_read_stream.hh 

maf_read_stream.hh : m_option.hh m_direction.hh 

maf_analyzer_missing.cc : maf_analyzer_missing.hh maf_read_stream.hh

maf_analyzer_missing.hh : maf_read_stream.hh

.PHONY : clean
clean :
	-rm $(OUTPUT) *.o
