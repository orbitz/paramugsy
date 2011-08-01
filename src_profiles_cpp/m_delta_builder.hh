#ifndef M_DELTA_BUILDER_HH
#define M_DELTA_BUILDER_HH

#include <m_delta.hh>
#include <m_metaprofile.hh>

namespace Para_mugsy {
  
  class M_delta_builder {
  public:
    M_delta_builder(std::pair<std::string, std::string> const& sequences,
                    std::pair<std::string, long> const& ref_header,
                    std::pair<std::string, long> const& query_header,
                    M_profile_idx const& ref_start,
                    M_profile_idx const& query_start,
                    M_metaprofile const& query_metaprofile) :
      sequences(sequences), ref_header(ref_header), query_header(query_header),
      ref_start(ref_start), ref_pos(ref_start), query_start(query_start), query_pos(query_start),
      query_metaprofile(query_metaprofile)
    {}

    void add_gap(strand_t strand, M_range<M_profile_idx> const& diff) {
      switch(strand) {
      case S_REF:
        long ref_len = ref_pos - ref_start;
        for(std::vector<M_range<M_profile_idx> >::const_iterator i = ref_gaps.begin();
            i != ref_gaps.end();
            ++i) {
          ref_len += i->length();
        }
        
        ref_gaps.push_back(M_range<M_profile_idx>(diff.get_start() + ref_len + 1,
                                                  diff.get_end() + ref_len + 1));
        ref_pos += diff.get_start();
        query_pos += diff.get_end() + 1;
        break;
      case S_QUERY:
        long query_len = query_pos - query_start;
        for(std::vector<M_range<M_profile_idx> >::const_iterator i = query_gaps.begin();
            i != query_gaps.end();
            ++i) {
          query_len += i->length();
        }
        
        query_gaps.push_back(M_range<M_profile_idx>(diff.get_start() + query_len + 1,
                                                    diff.get_end() + query_len + 1));
        ref_pos += diff.get_end() + 1;
        query_pos += diff.get_start();
        break;
      }
    }

    void add_offset(long offset) {
      ref_pos += offset;
      query_pos += offset;
    }

    M_option<M_delta_entry> to_delta() const;
    
  private:
    std::pair<std::string, std::string> sequences;
    /* delta_type_t delta_type; -- Ignoring this for now, assuming Nucmer */
    std::pair<std::string, long> ref_header;
    std::pair<std::string, long> query_header;

    M_profile_idx ref_start;
    M_profile_idx ref_pos;

    M_profile_idx query_start;
    M_profile_idx query_pos;
    M_metaprofile const& query_metaprofile;

    std::vector<M_range<M_profile_idx> > ref_gaps;
    std::vector<M_range<M_profile_idx> > query_gaps;
  };
  
}

#endif
