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
  // Maf_missing_report const &missing_report = maf_missing.genome_map();
  
  for(Maf_missing_report::const_iterator i = missing_report.begin();
      i != missing_report.end();
      ++i) {
    std::cout << "--------\n";
    for(std::vector<Maf_analyzer_missing_entry>::const_iterator missing_i = i->second.begin();
        missing_i != i->second.end();
        ++missing_i) {
      std::cout << i->first << "\t" << missing_i->range().get_start() << "\t" << missing_i->range().get_end() << std::endl;
    }
  }

  return 0;
}
