#include <istream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <cmath>

#include <algorithm>

#include <m_option.hh>
#include <m_profile.hh>
#include <m_range.hh>

namespace Para_mugsy {
  M_option<M_profile> read_profile_file(bool lite, std::istream &in_stream) {
    std::string line;

    if(std::getline(in_stream, line)) {
      /*
       * Elements we will read in
       */
      std::string p_major_name;
      std::string p_minor_name;
      std::string p_seq_name;
      M_seq_idx start;
      M_seq_idx end;
      unsigned int p_length;
      unsigned int p_src_size;
      std::vector<M_range<M_profile_idx> > p_gaps;
      std::string p_seq_text;


      std::istringstream iss(line);

      if(iss >>
         p_major_name >>
         p_minor_name >>
         p_seq_name >>
         start >>
         end >>
         p_length >>
         p_src_size) {


        M_range<M_seq_idx> p_range = M_range<M_seq_idx>(start, end);

        while(std::getline(in_stream, line) && line != "0") {
          std::istringstream iss(line);
          if(iss >> start >> end) {
            p_gaps.push_back(M_range<M_profile_idx>(start, end));
          }
          else {
            throw Profile_read_error();
          }
        }

        if(!lite) {
          std::getline(in_stream, p_seq_text);
        }
        else {
          /*
           * Even if we are lite we need to read the string in so the following
           * profile reads work
           */
          std::string throw_away;
          std::getline(in_stream, throw_away);
        }

        return M_option<M_profile>(M_profile(p_major_name,
                                             p_minor_name,
                                             p_seq_name,
                                             p_range,
                                             p_length,
                                             p_src_size,
                                             p_gaps,
                                             p_seq_text));
      }
      else {
        throw Profile_read_error();
      }
    }
    else {
      return M_option<M_profile>();
    }
  }

  M_option<M_profile> read_profile_file(std::istream &in_stream) {
    return read_profile_file(false, in_stream);
  }

  M_profile_idx profile_idx_of_seq_idx(M_profile const& p, M_seq_idx si) {
    M_seq_idx offset = std::abs(p.p_range.get_start() - si) + 1;
    if(p.p_range.contains(si)) {
      long gaps = 0;
      for(std::vector<M_range<M_profile_idx> >::const_iterator i = p.p_gaps.begin();
          i != p.p_gaps.end();
          ++i) {
        if(i->get_start() <= (offset + gaps)) {
          gaps += i->length();
        }
        else {
          goto return_idx;
        }
      }

    return_idx:
      return gaps + offset;
    }
    else {
      throw Seq_idx_out_of_range();
    }
  }

  M_option<M_seq_idx> seq_idx_of_profile_idx(M_profile const& p, M_profile_idx pi) {
    if(pi < p.p_length + 1) {
      long gaps = 0;
      for(std::vector<M_range<M_profile_idx> >::const_iterator i = p.p_gaps.begin();
          i != p.p_gaps.end();
          ++i) {
        if(i->get_end() < pi) {
          gaps += i->length();
        }
        else if(i->get_start() <= pi) {
          return M_option<M_seq_idx>();
        }
        else {
          goto return_idx;
        }
      }

    return_idx:
      long offset = pi - gaps - 1;
      switch(p.p_range.get_direction()) {
      case D_FORWARD:
        return M_option<M_seq_idx>(p.p_range.get_start() + offset);
      case D_REVERSE:
        return M_option<M_seq_idx>(p.p_range.get_start() - offset);
      }

      /*
       * C++ is too dumb to know I'm handling all directions up top, so
       * throwing an exception here so we never reach end of non-void function
       */
      throw std::exception();
    }
    else {
      throw Profile_idx_out_of_range();
    }
  }


  /*
   * Takes profile coordinates and returns a subset of the profile that corresponds to those coordinates.
   * Right now gaps ARE NOT translate back into zero index because the user of this code is legacy and
   * depends on that, although this should be changed soon.
   *
   * This will return None in the case that there is nothing but gaps in the range of s and e.
   * If s and e are not in ascending order they will be swapped.
   */
  M_option<M_profile> subset_profile(M_profile const &p, M_profile_idx s, M_profile_idx e) {
    if(s <= 0 || p.p_length < s || e <= 0 || p.p_length < e) {
      throw Profile_idx_out_of_range();
    }

    if(s > e) {
      std::swap(s, e);
    }

    M_range<M_profile_idx> profile_sub_range(s, e);
    std::vector<M_range<M_profile_idx> > gaps;
    for(std::vector<M_range<M_profile_idx> >::const_iterator i = p.p_gaps.begin();
        i != p.p_gaps.end();
        ++i) {
      if(M_option<M_range<M_profile_idx> > o = overlap(*i, profile_sub_range)) {
        gaps.push_back(o.value());
      }
    }

    if(!gaps.empty()) {
      if(gaps.size() == 1 && gaps[0].get_start() == s && gaps[0].get_end() == e) {
        /*
         * If the entire range is 1 gap, return empty
         */
        return M_option<M_profile>();
      }

      /*
       * Adjust the profile indecies where the actual sequences will start
       */
      if(gaps[0].get_start() == s) {
        s = gaps[0].get_end() + 1;
      }

      if(gaps[gaps.size() - 1].get_end() == e) {
        e = gaps[gaps.size() - 1].get_start() - 1;
      }
    }

    return M_option<M_profile>(M_profile(p.p_major_name,
                                         p.p_minor_name,
                                         p.p_seq_name,
                                         M_range<M_seq_idx>(seq_idx_of_profile_idx(p, s).value(),
                                                            seq_idx_of_profile_idx(p, e).value()),
                                         gaps));

  }

  M_profile subset_seq(M_profile const &p, M_seq_idx s, M_seq_idx e) {
    return subset_profile(p,
                          profile_idx_of_seq_idx(p, s),
                          profile_idx_of_seq_idx(p, e)).value();
  }

}
