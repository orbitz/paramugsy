/*
 * maf_analyzer takes a maf file and provides various pieces of information about it.
 */
#include <iostream>
#include <fstream>

#include <maf_read_stream.hh>
#include <maf_analyzer_missing.hh>

using namespace Para_mugsy;

int main(int argc, char **argv) {
  std::ifstream input_stream(argv[1]);
  Maf_read_stream maf_read_stream(input_stream);
  Maf_analyzer_missing maf_missing;
  
  while(M_option<Maf_entry> o_entry = maf_read_stream.next()) {
    maf_missing.add(o_entry.value());
  }

  Maf_missing_report missing_report = maf_missing.report();

  for(Maf_missing_report::const_iterator i = missing_report.begin();
      i != missing_report.end();
      ++i) {
    std::cout << "Genome: " << i->first << " Count: " << i->second.size() << std::endl;
  }

  return 0;
}
