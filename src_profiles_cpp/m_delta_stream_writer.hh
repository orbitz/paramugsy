#ifndef M_DELTA_STREAM_WRITER_HH
#define M_DELTA_STREAM_WRITER_HH

namespace Para_mugsy {

  inline void _push_ones(std::vector<long>& gaps, strand_t s, long ones) {
    long one = S_REF == s ? -1 : 1;
    for(; ones > 0; --ones) {
      gaps.push_back(one);
    }
  }
  

  inline std::vector<long> deltas_of_gaps(M_delta_entry const& de) {
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
  
  class M_delta_stream_writer {
  public:
    M_delta_stream_writer(std::ostream& out_stream) :
      out_stream(out_stream)
    {}
    
    void write(M_delta_entry const& de) {
      if(de.header_names != header_names) {
        out_stream << '>' << de.header_names.first << ' ' << de.header_names.second << ' ';
        out_stream << de.header_lengths.first << ' ' << de.header_lengths.second << '\n';
        
        header_names = de.header_names;
      }
      
      std::vector<long> gaps = deltas_of_gaps(de);
      out_stream << de.ref_range.get_start() << ' ' << de.ref_range.get_end() << ' ';
      out_stream << de.query_range.get_start() << ' ' << de.query_range.get_end() << " 1 2 3\n";
      for(std::vector<long>::const_iterator i = gaps.begin();
          i != gaps.end();
          ++i) {
        out_stream << *i << '\n';
      }
    }
    
  private:
    std::ostream& out_stream;
    std::pair<std::string, std::string> header_names;
  };

}

#endif
