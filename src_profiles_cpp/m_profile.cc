#include <iostream>

#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <cmath>

#include <m_option.hh>
#include <m_profile.hh>
#include <m_range.hh>

namespace Para_mugsy {
  M_profile read_profile_file(bool lite, std::string const& fname) {
    std::ifstream fin(fname.c_str());
    std::string line;

    if(std::getline(fin, line)) {
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

        while(std::getline(fin, line) && line != "0") {
          std::istringstream iss(line);
          if(iss >> start >> end) {
            p_gaps.push_back(M_range<M_profile_idx>(start, end));
          }
          else {
            throw Profile_read_error();
          }
        }

        if(!lite) {
          std::getline(fin, p_seq_text);
        }

        return M_profile(p_major_name,
                         p_minor_name,
                         p_seq_name,
                         p_range,
                         p_length,
                         p_src_size,
                         p_gaps,
                         p_seq_text);
      }
      else {
        throw Profile_read_error();
      }
    }
    else {
      throw Profile_read_error();
    }
    
        
  }

  M_profile read_profile_file(std::string const& fname) {
    return read_profile_file(false, fname);
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

  M_profile subset_profile(M_profile const& p, M_profile_idx s, M_profile_idx e) {
    (void)p;
    (void)s;
    (void)e;
    return M_profile("dummy",
                     "dummy",
                     "dummy",
                     M_range<M_seq_idx>(1, 2),
                     0,
                     0,
                     std::vector<M_range<M_profile_idx> >(),
                     "dummy");
  }
  
  M_profile subset_seq(M_profile const& p, M_seq_idx s, M_seq_idx e) {
    (void)p;
    (void)s;
    (void)e;    
    return M_profile("dummy",
                     "dummy",
                     "dummy",
                     M_range<M_seq_idx>(1, 2),
                     0,
                     0,
                     std::vector<M_range<M_profile_idx> >(),
                     "dummy");
  }

}
