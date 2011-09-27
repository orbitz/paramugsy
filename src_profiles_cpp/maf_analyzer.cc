/*
 * maf_analyzer takes a maf file and provides various pieces of information about it.
 */
#include <iostream>
#include <fstream>

#include <maf_read_stream.hh>

using namespace Para_mugsy;

int main(int argc, char **argv) {
  std::ifstream input_stream(argv[1]);
  Maf_read_stream maf_read_stream(input_stream);

  while(M_option<Maf_entry> o_entry = maf_read_stream.next()) {
    std::cout << "score = " << o_entry.value().score() << std::endl;
  }

  return 0;
}
