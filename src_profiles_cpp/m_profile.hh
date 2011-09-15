#ifndef M_PROFILE_HH
#define M_PROFILE_HH

#include <ostream>

#include <vector>
#include <string>

#include <m_option.hh>
#include <m_range.hh>

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

    M_profile(std::string const& p_major_name,
              std::string const& p_minor_name,
              std::string const& p_seq_name,
              M_range<M_seq_idx> const& p_range,
              std::vector<M_range<M_profile_idx> > const& p_gaps) :
      p_major_name(p_major_name),
      p_minor_name(p_minor_name),
      p_seq_name(p_seq_name),
      p_range(p_range),
      p_length(p_range.length()),
      p_src_size(0),
      p_gaps(p_gaps),
      p_seq_text("")
    {
      for(std::vector<M_range<M_profile_idx> >::const_iterator i = p_gaps.begin();
          i != p_gaps.end();
          ++i) {
        p_length += i->length();
      }
    }
    
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

    M_profile& operator=(M_profile const& p) {
      p_major_name = p.p_major_name;
      p_minor_name = p.p_minor_name;
      p_seq_name = p.p_seq_name;
      p_range = p.p_range;
      p_length = p.p_length;
      p_src_size = p.p_src_size;
      p_gaps = p.p_gaps;
      p_seq_text = p.p_seq_text;

      return *this;
    }
    
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

  M_option<M_profile> subset_profile(M_profile const& p, M_profile_idx s, M_profile_idx e);
  M_profile subset_seq(M_profile const& p, M_seq_idx s, M_seq_idx e);


  inline std::ostream& operator<<(std::ostream& out, M_profile const& p) {
    out << "p_major_name = " << p.p_major_name << "\n";
    out << "p_minor_name = " << p.p_minor_name << "\n";
    out << "p_seq_name = " << p.p_seq_name << "\n";
    out << "p_range = (" << p.p_range.get_start() << ", " << p.p_range.get_end() << ")\n";
    out << "p_length = " << p.p_length << "\n";
    out << "p_src_size = " << p.p_src_size << "\n";
    out << "p_gaps = \n";
    for(std::vector<M_range<M_profile_idx> >::const_iterator i = p.p_gaps.begin();
        i != p.p_gaps.end();
        ++i) {
      out << "(" << i->get_start() << ", " << i->get_end() << ") ";
    }
    out << "\n";
    return out;
  }
  
}

#endif
