#ifndef M_DELTA_HH
#define M_DELTA_HH

#include <istream>
#include <string>
#include <vector>

#include <m_option.hh>
#include <m_range.hh>
#include <m_profile.hh>

namespace Para_mugsy {
  class Delta_stream_parse_error : public std::exception {};

  enum strand_t { S_REF, S_QUERY };
  
  struct M_delta_entry {
    M_delta_entry(std::pair<std::string, std::string> const& header_names,
                  std::pair<long, long> const& header_lengths,
                  M_range<M_seq_idx> const& ref_range,
                  M_range<M_seq_idx> const& query_range,
                  std::vector<M_range<M_profile_idx> > const& ref_gaps,
                  std::vector<M_range<M_profile_idx> > const& query_gaps) :
      header_names(header_names),
      header_lengths(header_lengths),
      ref_range(ref_range),
      query_range(query_range),
      ref_gaps(ref_gaps),
      query_gaps(query_gaps)
    {}
    
    std::pair<std::string, std::string> const header_names;
    std::pair<long, long> const header_lengths;

    M_range<M_seq_idx> const ref_range;
    M_range<M_seq_idx> const query_range;

    std::vector<M_range<M_profile_idx> > const ref_gaps;
    std::vector<M_range<M_profile_idx> > const query_gaps;
  };

  class M_delta_stream {
  public:
    M_delta_stream(std::istream& ifstream);

    std::string const& stream_type() { return stream_type_; }
    std::pair<std::string, std::string> const& sequence_files() { return sequence_files_; }

    M_option<M_delta_entry> next();

  private:
    std::istream& in_stream_;
    std::string stream_type_;
    std::pair<std::string, std::string> sequence_files_;
    std::pair<std::string, std::string> header_names_;
    std::pair<long, long> header_lengths_;
  };
  
}

#endif
