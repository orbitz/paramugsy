#include <iostream>

#include <fstream>
#include <ostream>

#include <vector>
#include <string>
#include <map>

#include <cassert>

#include <sys/types.h>
#include <dirent.h>

#include <m_delta.hh>
#include <m_profile.hh>
#include <m_translate.hh>

using namespace Para_mugsy;

namespace {
  std::vector<std::string> _list_dir(std::string const& dir) {
    std::vector<std::string> ret;
    DIR *d = opendir(dir.c_str());

    if(d) {
      while(struct dirent *f = readdir(d)) {
        ret.push_back(dir + '/' + f->d_name);
      }
      closedir(d);
    }
    else {
      throw Read_dir_error();
    }
    return ret;
  }

  bool _is_not_idx(std::string const& f) {
    std::string::size_type s = f.find(".idx");
    return s != std::string::npos;
  }
  
  std::vector<std::string> _list_dir_idx(std::string const& dir) {
    std::vector<std::string> ret = _list_dir(dir);
    std::vector<std::string>::iterator new_last = remove_if(ret.begin(),
                                                            ret.end(),
                                                            _is_not_idx);
    ret.erase(ret.begin(), new_last);

    return ret;
  }

  std::map<std::string, std::vector<M_profile> > _profile_map_of_dir(std::string const& dir) {
    std::vector<std::string> files = _list_dir_idx(dir);
    std::map<std::string, std::vector<M_profile> > ret;

    for(std::vector<std::string>::const_iterator i = files.begin();
        i != files.end();
        ++i) {
      M_profile profile = read_profile_file(true, *i);
      ret[profile.p_seq_name].push_back(profile);
    }
    return ret;
  }


  M_delta_entry _reverse_if_needed(M_delta_entry const& de, M_profile const& left_profile) {
    if(de.ref_range.get_direction() != left_profile.p_range.get_direction()) {
      return de.reverse();
    }
    else {
      return de;
    }
  }
  
  void _generate_delta(M_delta_entry const& de,
                       M_profile const& left_profile,
                       M_range<M_seq_idx> const& ref_seq,
                       M_profile const& right_profile,
                       M_range<M_seq_idx> const& query_seq,
                       std::ostream& out_stream) {
    /*
     * We know that both profiles overlap our delta entry in some way but we do not know if
     * where they overlap the delta entry, the delta entry still overlaps itself.  That is to say
     * if I have a delta entry with the reference and query range of 0 - 10, what if my left
     * profile overlaps the reference range at 0 - 3 and my right profile overlaps the query range
     * at 6 - 10.  This combination of values no longer overlap each other in which case we have
     * no work to do.  On the other hand they overlaped at 4 - 8 and 6 - 10 we would have an overlap of
     * 6 - 8 that needed to be translated into the new delta.
     *
     * Our first step is to take the delta entry and create profiles of its reference and query sections.
     * `ref_seq` and `query_seq` are defined in terms of the original genome sequences so we convert those
     * to profile.  The ref and query profiles from delta entry are the same length so a point that is on
     * one will correspond to the same location on the other (when using profile indecies).  That means
     * with the profile indecies that `ref_seq` and `query_seq` have been mapped back to we can see
     * if they overlap.  If they do, we have a region to convert. 
     */
    M_profile d_ref_profile("",
                            "",
                            "",
                            de.ref_range,
                            de.ref_gaps);
    
    M_profile d_query_profile("",
                              "",
                              "",
                              de.query_range,
                              de.query_gaps);

    M_range<M_profile_idx> d_ref_range(profile_idx_of_seq_idx(d_ref_profile, ref_seq.get_start()),
                                       profile_idx_of_seq_idx(d_ref_profile, ref_seq.get_end()));
    M_range<M_profile_idx> d_query_range(profile_idx_of_seq_idx(d_query_profile, query_seq.get_start()),
                                         profile_idx_of_seq_idx(d_query_profile, query_seq.get_end()));

    if(M_option<M_range<M_profile_idx> > d_overlap = overlap(d_ref_range, d_query_range)) {
      /*
       * The location that the left and right profiles overlap the delta entry still allows the
       * reference and query in the delta entry to overlap.
       *
       * Now we need to translate the section, but first we need to know what we are translating. We need
       * two peices of information per profile: the starting location and the gaps for the range we are
       * translating.  We finally need the ending location for just one profile since it will correspond
       * to the ending location for all of them.
       *
       * There is one condition that will keep us from translating anything here: if the new area is all gaps
       * in the left or right profile.
       */
      
      M_option<M_profile> d_ref_profile_sub_o = subset_profile(d_ref_profile,
                                                               d_overlap.value().get_start(),
                                                               d_overlap.value().get_end());

      M_option<M_profile> d_query_profile_sub_o = subset_profile(d_query_profile,
                                                                 d_overlap.value().get_start(),
                                                                 d_overlap.value().get_end());

      if(d_ref_profile_sub_o && d_query_profile_sub_o) {
        M_profile const& d_ref_profile_sub = d_ref_profile_sub_o.value();
        M_profile const& d_query_profile_sub = d_query_profile_sub_o.value();

        M_profile ref_profile_sub = subset_seq(left_profile,
                                               d_ref_profile_sub.p_range.get_start(),
                                               d_ref_profile_sub.p_range.get_end());

        M_profile query_profile_sub = subset_seq(right_profile,
                                                 d_query_profile_sub.p_range.get_start(),
                                                 d_query_profile_sub.p_range.get_end());


        /*
         * The sub profiles for delta and profiles should be equal for the respective types
         */
        assert(d_ref_profile_sub.p_range.length() == ref_profile_sub.p_range.length() && d_query_profile_sub.p_range.length() == query_profile_sub.p_range.length());
      }
    }
  }
    

  
  void _translate_delta_with_profiles(M_profile const& left_profile,
                                      M_profile const& right_profile,
                                      M_delta_entry const& de,
                                      std::ostream& out_stream) {
    /*
     * We know that the profiles correspond to the names in the delta entry but
     * we don't know if these profiles actually overlap the delta entry at all.
     * If both profiles overlap our delta entry at all then we need to translate
     * the overlapped area back into the context of the delta sequences.  With
     * these values we call _generate_delta.
     */
    M_option<M_range<M_seq_idx> > ref_overlap_o = overlap(de.ref_range, left_profile.p_range);
    M_option<M_range<M_seq_idx> > query_overlap_o = overlap(de.query_range, right_profile.p_range);
    
    if(ref_overlap_o && query_overlap_o) {
      M_range<M_seq_idx> const& ref_overlap = ref_overlap_o.value();
      M_range<M_seq_idx> const& query_overlap = query_overlap_o.value();

      M_delta_entry n_de = _reverse_if_needed(de, left_profile);

      _generate_delta(n_de, left_profile, ref_overlap, right_profile, query_overlap, out_stream);
    }
  }
  
  
  void _translate_delta(std::map<std::string, std::vector<M_profile> > const& left_profile_map,
                        std::map<std::string, std::vector<M_profile> > const& right_profile_map,
                        M_delta_stream& ds,
                        std::ostream& out_stream) {
    while(M_option<M_delta_entry> d = ds.next()) {
      std::map<std::string, std::vector<M_profile> >::const_iterator left_profiles_i = left_profile_map.find(d.value().header_names.first);
      std::map<std::string, std::vector<M_profile> >::const_iterator right_profiles_i = right_profile_map.find(d.value().header_names.second);      

      if(left_profiles_i != left_profile_map.end() && right_profiles_i != right_profile_map.end()) {
        std::vector<M_profile> const& left_profiles = left_profiles_i->second;
        std::vector<M_profile> const& right_profiles = right_profiles_i->second;

        /*
         * For every combinatino of left and right profile that are for our genome
         * we call try to translate any overlapping area between the profiles and
         * the delta.
         *
         * We don't know which of the profile combinations will overlap,
         * _translate_delta_profiles will determine that, here we are simply
         * calling for every combination.  It is guaranteed that at least one
         * combination of profiles will overlap the delta entry but we don't know
         * which.  Some kind of interval tree might be a useful optimization here.
         */
        for(std::vector<M_profile>::const_iterator left_i = left_profiles.begin();
            left_i != left_profiles.end();
            ++left_i) {
          for(std::vector<M_profile>::const_iterator right_i = right_profiles.begin();
              right_i != right_profiles.end();
              ++right_i) {
            _translate_delta_with_profiles(*left_i, *right_i, d.value(), out_stream);
          }
        }
      }
    }
  }
}

namespace Para_mugsy {
  
  void translate(std::string const& left_dir,
                 std::string const& right_dir,
                 std::vector<std::string> const& nucmer_list,
                 std::ostream& out_stream) {
    std::map<std::string, std::vector<M_profile> > left_profile_map = _profile_map_of_dir(left_dir);
    std::map<std::string, std::vector<M_profile> > right_profile_map = _profile_map_of_dir(right_dir);

    for(std::vector<std::string>::const_iterator i = nucmer_list.begin();
        i != nucmer_list.end();
        ++i) {
      std::ifstream in_delta_stream(i->c_str());
      M_delta_stream ds(in_delta_stream);
      _translate_delta(left_profile_map, right_profile_map, ds, out_stream);
    }
  }

  
}
