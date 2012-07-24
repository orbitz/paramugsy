#include <vector>

#include <maf_analyzer_missing.hh>
#include <maf_read_stream.hh>

namespace {
  using namespace Para_mugsy;
  
  typedef std::vector<Maf_analyzer_missing_entry> Missing_vector;


  bool _adjacent_left(Missing_vector::iterator point, M_range<long> const &range) {
    Missing_vector::iterator previous_point = point - 1;
    return (previous_point->range().get_end() + 1) == range.get_start();
  }

  bool _adjacent_right(Missing_vector::iterator point, M_range<long> const &range) {
    return (range.get_end() + 1) == point->range().get_start();
  }

  bool _adjacent_left_right(Missing_vector::iterator point, M_range<long> const &range) {
    return _adjacent_left(point, range) && _adjacent_right(point, range);
  }
  
  Missing_vector::iterator _find_insertion_point(M_range<long> const &range,
                                                 Missing_vector &genome_missing) {
    for(Missing_vector::iterator i = genome_missing.begin();
        i != genome_missing.end();
        ++i) {
      if(range.get_end() < i->range().get_start()) {
        return i;
      }
    }

    return genome_missing.end();
  }
  
  void _insert(Maf_entry_alignment const &alignment, Maf_genome_map &genome_map) {
    Missing_vector &genome_missing = genome_map[alignment.genome_name()];
    M_range<long> range(alignment.range().abs());
    Missing_vector::iterator insertion_point = _find_insertion_point(range,
                                                                     genome_missing);


    if(genome_missing.empty()) {
      /*
       * No entries for this genome yet, so add our current one
       */
      Maf_analyzer_missing_entry missing_entry;
      missing_entry.set_range(range);
      genome_missing.insert(insertion_point, missing_entry);
    }
    else {
      /*
       * Just a quick check to make sure we aren't overlapping with anyone else
       */
      // if(!(genome_missing.empty() ||
      //      insertion_point == genome_missing.begin() ||
      //      (insertion_point - 1)->range().get_end() < alignment.range().get_start())) {
      //   std::cout << "OVERLAP: " << (insertion_point - 1)->range() << " " << alignment.range() << " " << alignment.genome_name() << "\n";
      // }

      if(insertion_point != genome_missing.begin() &&
         insertion_point != genome_missing.end() &&
         _adjacent_left_right(insertion_point, range)) {
        /*
         * Insertion point somewhere in the middle and connects two points already there
         */
        Missing_vector::iterator previous_point = insertion_point - 1;
        M_range<long> prev_range = insertion_point->range();
        insertion_point->set_range(M_range<long>(previous_point->range().get_start(),
                                                 insertion_point->range().get_end()));
        genome_missing.erase(previous_point);
      }
      else if(insertion_point != genome_missing.end() &&
              _adjacent_right(insertion_point, range)) {
        /*
         * Our insertion point is not at the end and connects our alignment
         */
        M_range<long> prev_range = insertion_point->range();
        insertion_point->set_range(M_range<long>(range.get_start(),
                                                 insertion_point->range().get_end()));
      }
      else if(insertion_point != genome_missing.begin() &&
              _adjacent_left(insertion_point, range)) {
        /*
         * Insertion point is not at the beginning and connects our alignment
         */
        Missing_vector::iterator previous_point = insertion_point - 1;
        M_range<long> prev_range = previous_point->range();
        previous_point->set_range(M_range<long>(previous_point->range().get_start(),
                                                range.get_end()));

      }
      else {
        /*
         * Not adjacent to anything, insert
         */
        Maf_analyzer_missing_entry missing_entry;
        missing_entry.set_range(range);
        genome_missing.insert(insertion_point, missing_entry);
      }
    }
  }

  void _add_missing(Missing_vector &report_missing, long genome_size, Missing_vector const &genome_missing) {
    if(genome_missing.empty()) {
      Maf_analyzer_missing_entry missing_entry;
      missing_entry.set_range(M_range<long>(1, genome_size));
      report_missing.push_back(missing_entry);
    }
    else {
      if(1 < genome_missing[0].range().get_start()) {
        Maf_analyzer_missing_entry missing_entry;
        missing_entry.set_range(M_range<long>(1, genome_missing[0].range().get_end() - 1));
        report_missing.push_back(missing_entry);
      }
      if(genome_missing.size() > 1) {
        for(Missing_vector::const_iterator i = genome_missing.begin() + 1;
            i != genome_missing.end() - 1;
            ++i) {
          Maf_analyzer_missing_entry missing_entry;
          missing_entry.set_range(M_range<long>((i - 1)->range().get_end() + 1,
                                                i->range().get_start() - 1));
          report_missing.push_back(missing_entry);
        }
      }
      if((genome_missing.end() - 1)->range().get_end() < genome_size) {
        Maf_analyzer_missing_entry missing_entry;
        missing_entry.set_range(M_range<long>((genome_missing.end() - 1)->range().get_end() + 1,
                                              genome_size));
        report_missing.push_back(missing_entry);
      }
    }
  }
}

namespace Para_mugsy {

  /*
   * Initial implementation just looks for any overlapping sequences
   */
  void Maf_analyzer_missing::add(Maf_entry const &maf_entry) {
    for(std::vector<Maf_entry_alignment>::const_iterator i = maf_entry.alignments_begin();
        i != maf_entry.alignments_end();
        ++i) {
      genome_sizes_[i->genome_name()] = i->src_size();
      _insert(*i, genome_map_);
    }
  }

  Maf_missing_report Maf_analyzer_missing::report() const {
    Maf_missing_report ret;
    for(Maf_genome_map::const_iterator genome_i = genome_map_.begin();
        genome_i != genome_map_.end();
        ++genome_i) {
      _add_missing(ret[genome_i->first], genome_sizes_.find(genome_i->first)->second, genome_i->second);
    }
    return ret;
  }
}
