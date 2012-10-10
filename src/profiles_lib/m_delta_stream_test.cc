#include <iostream>
#include <fstream>

#include <m_delta.hh>

using namespace std;
using namespace Para_mugsy;

void _push_ones(std::vector<long>& gaps, strand_t s, long ones) {
  long one = S_REF == s ? -1 : 1;
  for(; ones > 0; --ones) {
    gaps.push_back(one);
  }
}

std::vector<long> _deltas_of_gaps(M_delta_entry const& de) {
  std::vector<long> ret;
  std::vector<M_range<M_profile_idx> >::const_iterator ref_i = de.ref_gaps.begin();
  std::vector<M_range<M_profile_idx> >::const_iterator query_i = de.query_gaps.begin();
  long pos = 0;

  while(1) {
    if(ref_i != de.ref_gaps.end() && query_i != de.query_gaps.end()) {
      if(ref_i->get_start() < query_i->get_start()) {
        ret.push_back(-(ref_i->get_start() - pos));
        _push_ones(ret, S_REF, ref_i->length() - 1);
        pos = ref_i->get_end();
        ++ref_i;
      }
      else {
        ret.push_back(query_i->get_start() - pos);
        _push_ones(ret, S_QUERY, query_i->length() - 1);
        pos = query_i->get_end();
        ++query_i;
      }
    }
    else if(ref_i != de.ref_gaps.end()) {
      ret.push_back(-(ref_i->get_start() - pos));
      _push_ones(ret, S_REF, ref_i->length() - 1);
      pos = ref_i->get_end();
      ++ref_i;
    }
    else if(query_i != de.query_gaps.end()) {
      ret.push_back(query_i->get_start() - pos);
      _push_ones(ret, S_QUERY, query_i->length() - 1);
      pos = query_i->get_end();
      ++query_i;
    }
    else {
      ret.push_back(0);
      break;
    }
  }
  return ret;
}


int main(int argc, char **argv) {
  ifstream in_stream(argv[1]);
  M_delta_stream ds(in_stream);

  while(M_option<M_delta_entry> de = ds.next()) {
    cout << de.value().header_names.first << " " << de.value().header_names.second << endl;

    std::cout << "ref_range = (" << de.value().ref_range.get_start() << ", " << de.value().ref_range.get_end() << ")\n";

    std::vector<M_range<M_profile_idx> > const& ref_gaps = de.value().ref_gaps;
    std::cout << "ref_gaps\n";
    for(std::vector<M_range<M_profile_idx> >::const_iterator i = ref_gaps.begin();
        i != ref_gaps.end();
        ++i) {
      cout << "(" << i->get_start() << ", " << i->get_end() << ")" << endl;
    }

    std::cout << "query_range = (" << de.value().query_range.get_start() << ", " << de.value().query_range.get_end() << ")\n";
    std::vector<M_range<M_profile_idx> > const& query_gaps = de.value().query_gaps;
    std::cout << "query_gaps\n";
    for(std::vector<M_range<M_profile_idx> >::const_iterator i = query_gaps.begin();
        i != query_gaps.end();
        ++i) {
      cout << "(" << i->get_start() << ", " << i->get_end() << ")" << endl;
    }

    std::vector<long> deltas = _deltas_of_gaps(de.value());
    for(std::vector<long>::iterator i = deltas.begin();
        i != deltas.end();
        ++i) {
      std::cout << *i << "\n";
    }

  }

  return 0;
}


