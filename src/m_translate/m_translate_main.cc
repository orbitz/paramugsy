#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <algorithm>

#include <m_fileutils.hh>
#include <m_translate.hh>

int const LEFT_PROFILE_DIR = 1;
int const RIGHT_PROFILE_DIR = 2;
int const NUCMER_FILE_LIST = 3;
int const OUTPUT_DELTA_PATH = 4;

using std::cout;
using std::cerr;
using std::endl;

int main(int argc, char **argv) {
  std::ios_base::sync_with_stdio(false);

  if(argc < 5) {
    cerr << "Usage: m_translate <left_profile_dir> <right_profile_dir> <nucmer_file_list> <output_delta_path>" << endl;
    return 1;
  }

  std::vector<std::string> nucmer_list;
  std::ifstream in_stream(argv[NUCMER_FILE_LIST]);
  std::string line;

  while(std::getline(in_stream, line)) {
    nucmer_list.push_back(line);
  }

  std::ofstream out_stream(argv[OUTPUT_DELTA_PATH]);

  out_stream << (std::string(argv[LEFT_PROFILE_DIR]) + "/sequences.fasta") << " " <<
    (std::string(argv[RIGHT_PROFILE_DIR]) + "/sequences.fasta") << std::endl;
  out_stream << "NUCMER\n";

  Para_mugsy::translate(std::string(argv[LEFT_PROFILE_DIR]),
                        std::string(argv[RIGHT_PROFILE_DIR]),
                        nucmer_list,
                        out_stream);

  return 0;
}
