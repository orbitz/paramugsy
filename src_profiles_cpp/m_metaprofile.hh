#ifndef M_METAPROFILE_HH
#define M_METAPROFILE_HH

#include <m_profile.hh>

namespace Para_mugsy {
  class M_metaprofile {
  public:
    M_metaprofile(M_profile const& profile) : profile(profile), reversed(false) {}
    M_metaprofile(M_profile const& profile, bool reversed) : profile(profile), reversed(reversed) {}
    M_metaprofile(M_metaprofile const& mp): profile(mp.profile), reversed(mp.reversed) {}

    M_metaprofile reverse() { return M_metaprofile(profile, !reversed); }

    bool is_reversed() const { return reversed; }

    M_profile_idx profile_idx_of_profile_idx(M_profile_idx pi) const {
      if(!reversed) {
        return pi;
      }
      else {
        return profile.p_length - pi + 1;
      }
    }


    M_profile profile;
    bool reversed;
  };


  M_seq_idx inline seq_idx_of_profile_idx(M_metaprofile mp, M_profile_idx pi) {
    return seq_idx_of_profile_idx(mp.profile, mp.profile_idx_of_profile_idx(pi));
  }

  M_profile_idx inline profile_idx_of_seq_idx(M_metaprofile mp, M_seq_idx si) {
    return mp.profile_idx_of_profile_idx(profile_idx_of_seq_idx(mp.profile, si));
  }

}

#endif
