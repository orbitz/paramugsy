#include <iostream>

#include <fstream>
#include <ostream>

#include <vector>
#include <string>
#include <map>

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

  void _translate_delta_with_profiles(M_profile const& left_profile,
                                      M_profile const& right_profile,
                                      M_delta_entry const& d,
                                      std::ostream& out_stream) {
    M_option<M_range<M_profile_idx> > ref_overlap_o = overlap(d.ref_range, left_profile.p_range);
    M_option<M_range<M_profile_idx> > query_overlap_o = overlap(d.query_range, right_profile.p_range);
    if(ref_overlap_o && query_overlap_o) {
      M_range<M_profile_idx> ref_overlap = ref_overlap_o.value();
      M_range<M_profile_idx> query_overlap = query_overlap_o.value();
      std::cout << "Translating: " << left_profile.p_major_name << std::endl;
      std::cout << "& Translating: " << right_profile.p_major_name << std::endl;
      std::cout << "ref_overlap = (" << ref_overlap.get_start() << ", " << ref_overlap.get_end() << ")" << std::endl;
      std::cout << "query_overlap = (" << query_overlap.get_start() << ", " << query_overlap.get_end() << ")" << std::endl;
      std::cout << "d.ref_range = (" << d.ref_range.get_start() << ", " << d.ref_range.get_end() << ")" << std::endl;
      std::cout << "d.query_range = (" << d.query_range.get_start() << ", " << d.query_range.get_end() << ")" << std::endl;
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
