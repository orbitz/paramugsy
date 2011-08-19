#include <iostream>
#include <vector>
#include <algorithm>

#include <m_option.hh>
#include <m_delta.hh>
#include <m_delta_stream_writer.hh>

using namespace std;
using namespace Para_mugsy;

bool _sort_by_header(M_delta_entry const &l, M_delta_entry const &r) {
  return (l.header_names.first < r.header_names.first ||
          (l.header_names.first == r.header_names.first && l.header_names.second < r.header_names.second));
}

bool _sort_inner(M_delta_entry const &l, M_delta_entry const &r) {
  return (l.ref_range.get_start() < r.ref_range.get_start() ||
          (l.ref_range.get_start() == r.ref_range.get_start() && l.query_range.get_start() < r.query_range.get_start()));
}


void sort_delta_entries(vector<M_delta_entry> &delta_entries) {
  sort(delta_entries.begin(), delta_entries.end(), _sort_by_header);

  vector<M_delta_entry>::iterator s_start = delta_entries.begin();
  for(vector<M_delta_entry>::iterator s_end = s_start;
      s_end != delta_entries.end();
      ++s_end) {
    if(s_start->header_names != s_end->header_names) {
      sort(s_start, s_end - 1, _sort_inner);
      s_start = s_end;
    }
  }
}

int main() {
  M_delta_stream ds(cin);
  M_delta_stream_writer dsw(cout);

  vector<M_delta_entry> delta_entries;
  
  while(M_option<M_delta_entry> de_o = ds.next()) {
    delta_entries.push_back(de_o.value());
  }

  sort_delta_entries(delta_entries);

  for(std::vector<M_delta_entry>::const_iterator i = delta_entries.begin();
      i != delta_entries.end();
      ++i) {
    dsw.write(*i);
  }
  
}
