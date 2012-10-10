#ifndef MAF_STITCH_HH
#define MAF_STITCH_HH
#include <fstream>

#include <string>
#include <vector>
#include <algorithm>

#include <m_range.hh>
#include <maf_read_stream.hh>

namespace Para_mugsy {
  typedef int Maf_alignment_id;

  class Maf_alignment {
  public:
    class Maf_alignment_entry;

    Maf_alignment();
    Maf_alignment(std::streampos const &pos);
    Maf_alignment(Maf_alignment const &ma);

    Maf_alignment &operator=(Maf_alignment const &ma);


    void add_maf_entry(Maf_entry const &me);

    std::vector<Maf_alignment_entry> const &alignments() const {
      return alignments_;
    }

    std::streampos const &pos() const {
      return pos_;
    }


    class Maf_alignment_entry {
    public:
      Maf_alignment_entry() :
        range_(-1, -1), genome_name_()
      {}

      Maf_alignment_entry(M_range<long> const &range, std::string const &genome_name) :
        range_(range), genome_name_(genome_name)
      {}

      Maf_alignment_entry(Maf_alignment_entry const &mae) :
        range_(mae.range_), genome_name_(mae.genome_name_)
      {}

      Maf_alignment_entry &operator=(Maf_alignment_entry const &mae) {
        range_ = mae.range_;
        genome_name_ = mae.genome_name_;
        return *this;
      }

      M_range<long> const &range() const {
        return range_;
      }

      std::string const &genome_name() const {
        return genome_name_;
      }

    private:
      M_range<long> range_;
      std::string genome_name_;
    };



  private:
    std::vector<Maf_alignment_entry> alignments_;
    std::streampos pos_;
  };


  class M_genome_range {
  public:
    M_genome_range(M_range<long> const &range, Maf_alignment_id alignment_id) :
      range_(range), alignment_id_(alignment_id)
    {}

    M_genome_range(M_genome_range const &genome_range) :
      range_(genome_range.range_), alignment_id_(genome_range.alignment_id_)
    {}

    void swap(M_genome_range &genome_range) {
      using std::swap;
      swap(range_, genome_range.range_);
      swap(alignment_id_, genome_range.alignment_id_);
    }

    M_genome_range &operator=(M_genome_range const &genome_range) {
      M_genome_range copy(genome_range);
      swap(copy);
      return *this;
    }

    M_range<long> const &range() const {
      return range_;
    }

    Maf_alignment_id alignment_id() const {
      return alignment_id_;
    }

  private:
    M_range<long> range_;
    Maf_alignment_id alignment_id_;
  };

  inline void swap(M_genome_range &left, M_genome_range &right) {
    left.swap(right);
  }


  typedef std::vector<Maf_alignment> Maf_alignment_table;

  typedef std::vector<Maf_alignment_id> Maf_alignment_point;
  typedef std::vector<Maf_alignment_point> Maf_sequence_alignment;
  typedef std::vector<Maf_sequence_alignment> Maf_stitch;

  Maf_alignment_table alignment_table_of_ifstream(std::ifstream &in_stream);
  Maf_stitch maf_stitch_of_alignment_table(Maf_alignment_table const &maf_alignment_table);

}

#endif
