#include <vector>
#include <string>

#include <m_range.hh>

namespace Para_mugsy {
  class Seq_idx_out_of_range : public std::exception {};
  class Profile_idx_out_of_range : public std::exception {};
  class Seq_idx_invalid : public std::exception {};
  class Profile_idx_invalid : public std::exception {};

  /*
   * These should become phantom types
   */
  typedef unsigned int M_seq_idx;
  typedef unsigned int M_profile_idx;
  
  struct Profile {
    std::pair<std::string, std::string> p_name;
    std::string p_seq_name;
    M_range<M_seq_idx> p_range;
    unsigned in p_length;
    std::vector<M_range<M_profile_idx> > p_gaps;
    unsigned int p_src_size;
    std::string p_seq_text;

    Profile reverse();
  };

  Profile read_profile_file(bool lite, std::string const& fname);

  Profile read_profile_file(std::string const& fname) {
    return read_profile_file(false, fname);
  }

  M_seq_idx seq_idx_of_int(unsigned int i) { return i; }
  M_profile_idx profile_idx_of_int(unsigned int i) { return i; }

  unsigned int int_of_seq_idx(M_seq_idx i) { return i; }
  unsigned int int_of_profile_idx(M_profile_idx i) { return i; }

  M_profile_idx profile_idx_of_seq_idx(Profile const& p, M_seq_idx si);
  M_seq_idx seq_idx_of_profile_idx(Profile const& p, M_profile_idx pi);

  Profile subset_profile(Profile const& p, M_profile_idx s, M_profile_idx e);
  Profile subset_seq(Profile const& p, M_seq_idx s, M_seq_idx e);
  
}
