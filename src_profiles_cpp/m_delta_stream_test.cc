#include <iostream>
#include <fstream>

#include <m_delta.hh>

using namespace std;
using namespace Para_mugsy;

int main(int argc, char **argv) {
  ifstream in_stream(argv[1]);
  Delta_stream ds(in_stream);

  while(M_option<Delta_entry> de = ds.next()) {
    cout << de.value().header_names.first << " " << de.value().header_names.second << endl;
    std::vector<M_range<M_profile_idx> > const& ref_gaps = de.value().ref_gaps;
    for(std::vector<M_range<M_profile_idx> >::const_iterator i = ref_gaps.begin();
        i != ref_gaps.end();
        ++i) {
      cout << "(" << i->get_start() << ", " << i->get_end() << ")" << endl;
    }
  }

  return 0;
}

  
