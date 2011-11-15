#ifdef MAF_STITCH_HH
#define MAF_STITCH_HH
#include <ifstream>

#include <vector>
#include <algorithm>

#include <m_range.hh>

namespace Para_mugsy {
  class Maf_alignment_entry {
    Maf_alignment_entry(Maf_entry const &maf_entry, int pos) :
      maf_entry_(maf_entry), pos_(pos)
    {}

    Maf_alignment_entry(Maf_alignment_entry const &mae) :
      maf_entry_(mae.maf_entry_), pos_(mae.pos_)
    {}

    void swap(Maf_alignment_entry &mae) {
      using std::swap;
      swap(maf_entry_, mae.maf_entry_);
      swap(pos_, mae.pos_);
    }

    M_genome_range &operator=(M_alignment_entry const &mae) {
      M_alignment_entry copy(mae);
      swap(copy);
      return *this;
    }

    Maf_entry const &maf_entry() {
      return maf_entry;
    }

    int pos() {
      return pos;
    }

  private:
    Maf_entry maf_entry_;
    int pos_;
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
    
    M_range<long> const &range() {
      return range_;
    }

    Maf_alignment_id alignment_id() {
      return alignment_id_;
    }

  private:
    M_range<long> range_;
    Maf_alignment_id alignment_id_;
  };

  inline void swap(Maf_alignment_entry &left, Maf_alignment_entry &right) {
    left.swap(right);
  }
  
  inline void swap(M_genome_range &left, M_genome_range &right) {
    left.swap(right);
  }


  typedef std::vector<Maf_alignment_entry> Maf_alignment_table;
  typedef std::vector<std::vector<Maf_alignment_id> > Maf_stitch;
  
  Maf_alignemnt_table alignment_table_of_ifstream(std::ifstream &in_stream);
  Maf_stitch maf_stitch_of_alignment_table(Maf_alignment_table const &maf_alignment_table);
  
}

#endif
