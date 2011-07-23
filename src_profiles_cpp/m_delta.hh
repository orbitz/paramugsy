#include <istream>
#include <string>
#include <vector>

#include <m_option.hh>
#include <m_range.hh>
#include <m_profile.hh>

namespace Para_mugsy {
  class Delta_stream_parse_error : public std::exception {};



  
  struct Delta_entry {
    std::pair<std::string> header_names;
    std::pair<unsigned int> header_lengths;

    M_range<M_seq_idx> ref_range;
    M_range<M_seq_idx> query_range;

    std::vector<M_range<M_profile_idx> > ref_gaps;
    std::vector<M_range<M_profile_idx> > query_gaps;
  };

  class Delta_stream {
  public:
    Delta_stream(std::istream&);

    std::string const& stream_type();
    std::pair<std::string> const& sequence_files();

    M_option<Delta_entry> next();

  private:
    std::string stream_type_;
    std::pair<std::string> sequence_files_;
  };
  
}
