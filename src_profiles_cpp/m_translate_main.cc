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

bool _is_not_fasta(std::string const& f) {
  std::string::size_type s = f.find(".fasta");
  return s == std::string::npos;
}

int main(int argc, char **argv) {
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

  std::vector<std::string> left_files(Para_mugsy::list_dir(argv[LEFT_PROFILE_DIR]));
  std::vector<std::string> right_files(Para_mugsy::list_dir(argv[RIGHT_PROFILE_DIR]));

  std::vector<std::string>::iterator left_new_end = std::remove_if(left_files.begin(),
                                                                   left_files.end(),
                                                                   _is_not_fasta);
  left_files.erase(left_new_end, left_files.end());

  std::vector<std::string>::iterator right_new_end = std::remove_if(right_files.begin(),
                                                                    right_files.end(),
                                                                    _is_not_fasta);
  
  right_files.erase(right_new_end, right_files.end());

  if(left_files.empty() || right_files.empty()) {
    std::cerr << "Profile directions must contain a fasta file" << std::endl;
    return -1;
  }

  out_stream << left_files[0] << " " << right_files[0] << std::endl;
  out_stream << "NUCMER\n";
  
  Para_mugsy::translate(std::string(argv[LEFT_PROFILE_DIR]),
                        std::string(argv[RIGHT_PROFILE_DIR]),
                        nucmer_list,
                        out_stream);
  
  
  return 0;
}
