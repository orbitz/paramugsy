#include <iostream>

#include <maf_stitch.hh>

using namespace Para_mugsy;

int main(int argc, char **argv) {
  if(argc != 2) {
    std::cerr << "Must be called with name of maf file\n";
    return 1;
  }

  std::ifstream in_maf(argv[1]);


  Maf_alignment_table mat = alignment_table_of_ifstream(in_maf);

  maf_stitch_of_alignment_table(mat);
  
  return 0;
}
