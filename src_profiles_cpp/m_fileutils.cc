#include <vector>
#include <string>

#include <sys/types.h>
#include <dirent.h>

#include <m_fileutils.hh>

namespace Para_mugsy {
  std::vector<std::string> list_dir(std::string const& dir) {
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
}
