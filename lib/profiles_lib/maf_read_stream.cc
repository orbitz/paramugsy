#include <sstream>
#include <string>

#include <maf_read_stream.hh>

namespace Para_mugsy {
  M_option<Maf_entry> Maf_read_stream::next() {
    std::string line;

    /*
     * Drop all comments and empty lines
     */
    while(std::getline(istream_, line) && ('#' == line[0] || line.empty()));

    if(!istream_.eof()) {
      if('a' == line[0]) {
        std::istringstream iss(line);
        char dummy;

        // Remove initial 's'
        iss >> dummy;

        std::string score;
        std::string label;

        if(iss >> score >> label) {
          Maf_entry maf_entry(score, label);

          while(std::getline(istream_, line) && 's' == line[0]) {
            maf_entry.add_alignment(Maf_entry_alignment(line));
          }
          return M_option<Maf_entry>(maf_entry);
        }
        else {
          throw Maf_parse_error();
        }
      }
      else {
        return M_option<Maf_entry>();
      }
    }
    else {
      return M_option<Maf_entry>();
    }
  }

}
