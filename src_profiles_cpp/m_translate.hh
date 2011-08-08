#ifndef M_TRANSLATE_HH
#define M_TRANSLATE_HH

#include <vector>
#include <string>
#include <ostream>

namespace Para_mugsy {
  void translate(std::string const& left_dir,
                 std::string const& right_dir,
                 std::vector<std::string> const& nucmer_list,
                 std::ostream& out_stream);
}

#endif
