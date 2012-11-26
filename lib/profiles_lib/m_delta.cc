#include <iostream>

#include <istream>
#include <sstream>
#include <string>

#include <m_delta.hh>

using namespace Para_mugsy;

namespace {
  typedef std::pair<strand_t, M_range<M_profile_idx> > gap_range;

  M_option<gap_range> _read_gap_range(std::vector<long>::const_iterator& curr,
                                      std::vector<long>::const_iterator& end,
                                      long offset) {
    if(curr != end) {
      long next_gap = *curr;
      strand_t strand = next_gap > 0 ? S_QUERY : S_REF;
      long gap_offset = next_gap > 0 ? next_gap : -next_gap;
      offset += gap_offset;
      ++curr;

      long gap_length = 0;
      for(; curr != end && (1 == *curr || -1 == *curr); ++curr) {
        long next_gap = *curr;
        strand_t curr_strand = next_gap > 0 ? S_QUERY : S_REF;
        if(strand != curr_strand) {
          break;
        }

        gap_length += 1;
      }

      return M_option<gap_range>(std::make_pair(strand,
                                                M_range<M_profile_idx>(offset, offset + gap_length)));
    }
    else {
      return M_option<gap_range>();
    }
  }

  /*
   * We want to turn something like:
   * 106 -6 1797 -9 -9 -1 7 1
   * Into:
   * Ref gaps: (112, 112) (1918, 1918) (1927, 1928)
   * Query gaps: (106, 106) (1909, 1909) (1935, 1936)
   */
  void _split_gaps(std::vector<long> const& gaps,
                   std::vector<M_range<M_profile_idx> >& ref_gaps,
                   std::vector<M_range<M_profile_idx> >& query_gaps) {
    std::vector<long>::const_iterator curr = gaps.begin();
    std::vector<long>::const_iterator end = gaps.end();
    M_profile_idx offset = 0;

    while(M_option<gap_range> gap = _read_gap_range(curr, end, offset)) {
      offset = gap.value().second.get_end();
      switch(gap.value().first) {
      case S_REF:
        ref_gaps.push_back(gap.value().second);
        break;
      case S_QUERY:
        query_gaps.push_back(gap.value().second);
        break;
      }
    }
  }
}

namespace Para_mugsy {
  M_delta_stream::M_delta_stream(std::istream& in_stream) : in_stream_(in_stream) {
    std::string line;

    if(std::getline(in_stream_, line)) {
      std::istringstream iss(line);
      std::string seq1;
      std::string seq2;
      if(iss >> seq1 >> seq1) {
        sequence_files_ = std::make_pair(seq1, seq1);
        if(!std::getline(in_stream_, stream_type_)) {
          throw Delta_stream_parse_error();
        }
      }
      else {
        throw Delta_stream_parse_error();
      }
    }
    else {
      throw Delta_stream_parse_error();
    }
  }

  M_delta_entry M_delta_entry::reverse() const {
    std::vector<M_range<M_profile_idx> > ref_gaps_new;
    std::vector<M_range<M_profile_idx> > query_gaps_new;
    M_range<M_seq_idx> ref_range_new(ref_range.reverse());
    M_range<M_seq_idx> query_range_new(query_range.reverse());

    /*
     * Get the actual length of the reference region profile
     */
    long ref_p_length = ref_range.length();
    for(std::vector<M_range<M_profile_idx> >::const_iterator i = ref_gaps.begin();
        i != ref_gaps.end();
        ++i) {
      ref_p_length += i->length();
    }

    /*
     * Flip gaps and adjust their position inside the profile
     */
    for(std::vector<M_range<M_profile_idx> >::const_reverse_iterator ri = ref_gaps.rbegin();
        ri != ref_gaps.rend();
        ++ri) {
      ref_gaps_new.push_back(M_range<M_profile_idx>(ref_p_length - ri->get_end() + 1,
                                                    ref_p_length - ri->get_start() + 1));
    }

    /*
     * Get the actual length of the reference region profile
     */
    long query_p_length = query_range.length();
    for(std::vector<M_range<M_profile_idx> >::const_iterator i = query_gaps.begin();
        i != query_gaps.end();
        ++i) {
      query_p_length += i->length();
    }

    /*
     * Flip gaps an adjust their position inside the profile
     */
    for(std::vector<M_range<M_profile_idx> >::const_reverse_iterator ri = query_gaps.rbegin();
        ri != query_gaps.rend();
        ++ri) {
      query_gaps_new.push_back(M_range<M_profile_idx>(query_p_length - ri->get_end() + 1,
                                                      query_p_length - ri->get_start() + 1));
    }

    return M_delta_entry(header_names,
                         header_lengths,
                         ref_range_new,
                         query_range_new,
                         ref_gaps_new,
                         query_gaps_new);
  }

  M_option<M_delta_entry> M_delta_stream::next() {
    std::string line;

    if(std::getline(in_stream_, line)) {
      std::istringstream iss(line);

      if(line[0] == '>') {
        /*
         * We have a new alignment header
         * Read prefixed >
         */
        char _dummy;
        iss >> _dummy;

        if(iss >> header_names_.first >> header_names_.second >> header_lengths_.first >> header_lengths_.second) {
          /*
           * We have read the alignment header in, now read the next alignment line for the next section
           */
          if(!std::getline(in_stream_, line)) {
            throw Delta_stream_parse_error();
          }
        }
        else {
          throw Delta_stream_parse_error();
        }
      }

      iss.clear();
      iss.str(line);
      long ref_start;
      long ref_end;
      long query_start;
      long query_end;
      long _error_1;
      long _error_2;
      long _error_3;

      if(iss >> ref_start >> ref_end >> query_start >> query_end >> _error_1 >> _error_2 >> _error_3) {
        std::vector<long> gaps;
        while(std::getline(in_stream_, line) && line != "0") {
          std::istringstream iss(line);
          int gap;
          if(iss >> gap) {
            gaps.push_back(gap);
          }
          else {
            throw Delta_stream_parse_error();
          }
        }

        M_range<M_seq_idx> ref_range(ref_start, ref_end);
        M_range<M_seq_idx> query_range(query_start, query_end);

        std::vector<M_range<M_profile_idx> > ref_gaps;
        std::vector<M_range<M_profile_idx> > query_gaps;

        _split_gaps(gaps, ref_gaps, query_gaps);

        return M_option<M_delta_entry>(M_delta_entry(header_names_,
                                                     header_lengths_,
                                                     ref_range,
                                                     query_range,
                                                     ref_gaps,
                                                     query_gaps));
      }
      else {
        throw Delta_stream_parse_error();
      }
    }
    else {
      return M_option<M_delta_entry>();
    }
  }
}
