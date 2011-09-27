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

namespace Para_mugsy {
  typedef std::map<std::string, std::vector<Maf_analyzer_missing_entry> > Maf_genome_map;
  /*
   * A report is the same as a genome map but it will have the inverse information
   */
  typedef Maf_genome_map Maf_missing_report;
  
  class Maf_analyzer_missing_entry {
  public:
    void set_range(M_range<long> const &range) {
      range_ = range;
    }

    M_range<long> range() const {
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

    Maf_missing_report report() const;
    
  private:
    Maf_genome_map genome_map;
  };
}
 
#endif
