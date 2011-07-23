#include <string>

#include <m_profile.hh>

namespace Para_mugsy {
  Profile read_profile_file(bool lite, std::string const& fname) {
  }


  M_profile_idx profile_idx_of_seq_idx(Profile const& p, M_seq_idx si) {
  }
  
  M_seq_idx seq_idx_of_profile_idx(Profile const& p, M_profile_idx pi) {
  }

  Profile subset_profile(Profile const& p, M_profile_idx s, M_profile_idx e) {
  }
  
  Profile subset_seq(Profile const& p, M_seq_idx s, M_seq_idx e) {
  }

}
