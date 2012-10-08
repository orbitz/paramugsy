#include <m_option.hh>
#include <m_delta.hh>
#include <m_metaprofile.hh>
#include <m_delta_builder.hh>

namespace Para_mugsy {
  M_option<M_delta_entry> M_delta_builder::to_delta() const {
    if(ref_start != ref_pos && query_start != query_pos) {
      M_profile_idx query_start = query_metaprofile.profile_idx_of_profile_idx(this->query_start);
      M_profile_idx query_pos = query_metaprofile.profile_idx_of_profile_idx(this->query_pos - 1);
      
      return M_option<M_delta_entry>(M_delta_entry(std::make_pair(ref_header.first, query_header.first),
                                                   std::make_pair(ref_header.second, query_header.second),
                                                   M_range<M_profile_idx>(ref_start, ref_pos - 1),
                                                   M_range<M_profile_idx>(query_start, query_pos),
                                                   ref_gaps,
                                                   query_gaps));
    }
    else {
      return M_option<M_delta_entry>();
    }
  }
}
