#ifndef M_PROFILE_HH
#define M_PROFILE_HH

#include <vector>
#include <string>

#include <m_option.hh>
#include <m_range.hh>
#include <m_profile.hh>

namespace Para_mugsy {
  class Profile_read_error : public std::exception {};
  class Seq_idx_out_of_range : public std::exception {};
  class Profile_idx_out_of_range : public std::exception {};
  class Seq_idx_invalid : public std::exception {};
  class Profile_idx_invalid : public std::exception {};

  /*
   * These should become phantom types
   */
  typedef long M_seq_idx;
  typedef long M_profile_idx;
  
  struct M_profile {
    M_profile(std::string const& p_major_name,
              std::string const& p_minor_name,
              std::string const& p_seq_name,
              M_range<M_seq_idx> const& p_range,
              long const& p_length,
              long const& p_src_size,
              std::vector<M_range<M_profile_idx> > const& p_gaps,
              std::string const& p_seq_text) :
      p_major_name(p_major_name),
      p_minor_name(p_minor_name),
      p_seq_name(p_seq_name),
      p_range(p_range),
      p_length(p_length),
      p_src_size(p_src_size),
      p_gaps(p_gaps),
      p_seq_text(p_seq_text)
    {}

    M_profile(M_profile const& p) :
      p_major_name(p.p_major_name),
      p_minor_name(p.p_minor_name),
      p_seq_name(p.p_seq_name),
      p_range(p.p_range),
      p_length(p.p_length),
      p_src_size(p.p_src_size),
      p_gaps(p.p_gaps),
      p_seq_text(p.p_seq_text)
    {}
    
    std::string p_major_name;
    std::string p_minor_name;
    std::string p_seq_name;
    M_range<M_seq_idx> p_range;
    long p_length;
    long p_src_size;
    std::vector<M_range<M_profile_idx> > p_gaps;
    std::string p_seq_text;

    M_profile reverse() const;
  };

  M_profile read_profile_file(bool lite, std::string const& fname);
  M_profile read_profile_file(std::string const& fname);

  M_seq_idx inline seq_idx_of_int(long i) { return i; }
  M_profile_idx inline profile_idx_of_int(long i) { return i; }

  long inline int_of_seq_idx(M_seq_idx i) { return i; }
  long inline int_of_profile_idx(M_profile_idx i) { return i; }

  M_profile_idx profile_idx_of_seq_idx(M_profile const& p, M_seq_idx si);
  M_option<M_seq_idx> seq_idx_of_profile_idx(M_profile const& p, M_profile_idx pi);

  M_profile subset_profile(M_profile const& p, M_profile_idx s, M_profile_idx e);
  M_profile subset_seq(M_profile const& p, M_seq_idx s, M_seq_idx e);
  
}

#endif
