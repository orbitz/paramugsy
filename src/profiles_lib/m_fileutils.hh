#ifndef M_FILEUTILS_HH
#define M_FILEUTILS_HH

#include <vector>
#include <string>

namespace Para_mugsy {
  class Read_dir_error : public std::exception {};

  std::vector<std::string> list_dir(std::string const& dir);
}

#endif
