#ifndef MAF_ANALYZER_MISSING_HH
#define MAF_ANALYZER_MISSING_HH
/*
 * In this we will be analyzing a maf file for any pieces of the genome
 * that are not represented in the MAF file.
 *
 * This can be done incrementally, as in entries can be continually added
 * and the current usage determined.
 *
 * This is done by maintaining a set of ranges for each genome of seen bases.  The range
 * also contains what genomes saw that particular range.  Overlapping regions can be
 * joined or split
 */
#include <map>
#include <vector>
#include <string>
#include <set>
#include <algorithm>

#include <maf_read_stream.hh>

namespace Para_mugsy {
  class Maf_analyzer_missing_entry;

  typedef std::map<std::string, std::vector<Maf_analyzer_missing_entry> > Maf_genome_map;
  /*
   * A report is the same as a genome map but it will have the inverse information
   */
  typedef Maf_genome_map Maf_missing_report;

  class Maf_analyzer_missing_entry {
  public:
    Maf_analyzer_missing_entry() :
      range_(-1, -1)
    {}

    Maf_analyzer_missing_entry(Maf_analyzer_missing_entry const &rhs) :
      genomes_(rhs.genomes_), range_(rhs.range_)
    {}

    Maf_analyzer_missing_entry &operator=(Maf_analyzer_missing_entry const &rhs) {
      Maf_analyzer_missing_entry copy(rhs);
      swap(copy);
      return *this;
    }

    void swap(Maf_analyzer_missing_entry &rhs) {
      using std::swap;
      swap(genomes_, rhs.genomes_);
      swap(range_, rhs.range_);
    }

    void set_range(M_range<long> const &range) {
      range_ = range;
    }

    M_range<long> const &range() const {
      return range_;
    }

    void set_genomes(std::set<std::string> const &genomes) {
      genomes_ = genomes;
    }

    std::set<std::string> &genomes() {
      return genomes_;
    }

    std::set<std::string> const &genomes() const {
      return genomes_;
    }


  private:
    std::set<std::string> genomes_;
    M_range<long> range_;
  };

  class Maf_analyzer_missing {
  public:
    void add(Maf_entry const &maf_entry);

    Maf_genome_map const &genome_map() { return genome_map_; }

    Maf_missing_report report() const;

  private:
    Maf_genome_map genome_map_;
    std::map<std::string, long> genome_sizes_;
  };
}

#endif
