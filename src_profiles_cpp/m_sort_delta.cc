#include <iostream>
#include <vector>
#include <string>
#include <algorithm>

#include <m_option.hh>
#include <m_delta.hh>
#include <m_delta_stream_writer.hh>

using namespace std;
using namespace Para_mugsy;

template <typename T, size_t N>
size_t array_length(T const (&_)[N]) {
  (void)_;
  return N;
}

template <typename T>
bool _less_than(T left_start, T left_end, T right_start, T right_end) {
  while(left_start != left_end && right_start != right_end) {
    if(*left_start < *right_start) {
      return true;
    }
    else if(*left_start == *right_start) {
      ++left_start;
      ++right_start;
    }
    else {
      return false;
    }
  }

  return false;
}

bool _sort_by_header(M_delta_entry const &l, M_delta_entry const &r) {
  std::string l_array[] = {l.header_names.first, l.header_names.second};
  std::string r_array[] = {r.header_names.first, r.header_names.second};

  return _less_than(&l_array[0],
                    &l_array[0] + array_length(l_array),
                    &r_array[0],
                    &r_array[0] + array_length(r_array));
}

bool _sort_inner(M_delta_entry const &l, M_delta_entry const &r) {
  M_seq_idx l_array[] = {l.ref_range.get_start(), l.query_range.get_start(), l.ref_range.get_end(), l.query_range.get_end()};
  M_seq_idx r_array[] = {r.ref_range.get_start(), r.query_range.get_start(), r.ref_range.get_end(), r.query_range.get_end()};

  return _less_than(&l_array[0],
                    &l_array[0] + array_length(l_array),
                    &r_array[0],
                    &r_array[0] + array_length(r_array));
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
  sort(s_start, delta_entries.end(), _sort_inner);
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
