#include <iostream>

#include <fstream>
#include <vector>
#include <string>
#include <map>

#include <m_option.hh>
#include <maf_stitch.hh>


namespace {
  using namespace Para_mugsy;

  int const ALIGNMENT_DISTANCE = 1;
  
  typedef std::map<std::string, std::vector<M_genome_range> > Sorted_genomes;
  typedef Maf_alignment::Maf_alignment_entry Maf_alignment_entry;

  bool genome_range_sort_comparator(M_genome_range const &left, M_genome_range const &right) {
    return left.range().abs().get_start() < right.range().abs().get_start();
  }

  
  Sorted_genomes sort_genomes(Maf_alignment_table const &mat) {
    Sorted_genomes ret;

    for(size_t i = 0; i < mat.size(); ++i) {
      for(std::vector<Maf_alignment_entry>::const_iterator alignment = mat[i].alignments().begin();
          alignment != mat[i].alignments().end();
          ++alignment) {
        ret[alignment->genome_name()].push_back(M_genome_range(alignment->range(), i));
      }
    }

    for(_Sorted_genomes::iterator i = ret.begin();
        i != ret.end();
        ++i) {
      std::sort(i->second.begin(), i->second.end(), genome_range_sort_comparator);
    }

    return ret;
  }

  M_option<Maf_alignment_entry> find_alignment_entry(std::vector<Maf_alignment_entry> const &alignments,
                                                                     std::string const &genome_name) {
    for(std::vector<Maf_alignment_entry>::const_iterator i = alignments.begin();
        i != alignments.end();
        ++i) {
      if(i->genome_name() == genome_name) {
        return M_option<Maf_alignment_entry>(*i);
      }
    }

    return M_option<Maf_alignment_entry>();
  }

  bool in_range(long diff, long epsilon) {
    return diff >= 0 && diff <= epsilon;
  }

  long abs(long diff) {
    return diff < 0 ? -diff : diff;
  }
  
  bool is_alignment_adjacent(std::vector<Maf_alignment_entry> const &left_alignments,
                              std::vector<Maf_alignment_entry> const &right_alignments) {
    if(left_alignments.size() == right_alignments.size()) {
      std::cout << "----------\n";
      for(std::vector<Maf_alignment_entry>::const_iterator i = left_alignments.begin();
          i != left_alignments.end();
          ++i) {
        std::cout << "Checking: " << i->genome_name() << "\n";
        M_range<long> const &left_range = i->range();
        if(M_option<Maf_alignment_entry> mae_o = find_alignment_entry(right_alignments,
                                                                       i->genome_name())) {
          M_range<long> const &right_range = mae_o.value().range();
          bool directions_equal = left_range.get_direction() == right_range.get_direction();
          bool forward_and_adjacent = left_range.get_direction() == D_FORWARD && in_range(right_range.get_start() - left_range.get_end(), ALIGNMENT_DISTANCE);
          bool reverse_and_adjacent = left_range.get_direction() == D_REVERSE && in_range(left_range.get_end() - right_range.get_start(), ALIGNMENT_DISTANCE);
          std::cout << "Comparing " << left_range << " ~ " << right_range << " " << abs(left_range.get_end() - right_range.get_start()) << " ";
          if(directions_equal && (forward_and_adjacent || reverse_and_adjacent)) {
            std::cout << "true\n";
          }
          else {
            std::cout << "false\n";
            return false;
          }
        }
        else {
          return false;
        }
      }
      std::cout << "All match\n";
      return true;
    }
    else {
      return false;
    }
  }
  
  
  void find_adjacent_alignments(std::vector<M_genome_range> const &genome, Maf_alignment_table const &mat) {
    std::cout << "genome.size() = " << genome.size() << "\n";
    if(genome.size() > 1) {
      for(std::vector<M_genome_range>::const_iterator i = genome.begin();
          i != genome.end() - 1;
          ++i) {
        Maf_alignment_id curr_id = i->alignment_id();
        Maf_alignment_id next_id = (i + 1)->alignment_id();
        std::vector<Maf_alignment_entry> const &curr_alignments = mat[curr_id].alignments();
        std::vector<Maf_alignment_entry> const &next_alignments = mat[next_id].alignments();
        std::cout << "Checking genome range " << i->range() << " against " << (i + 1)->range() << "\n";
        if(_is_alignment_adjacent(curr_alignments, next_alignments)) {
          std::cout << "MATCH: " << curr_id << " ~ " << next_id << "\n";
        }
      }
    }
  }
  
  void print_adjacent_alignments(_Sorted_genomes const &sorted_genomes, Maf_alignment_table const &mat) {
    std::vector<std::vector<M_genome_range> > genome_ranges;
    
    for(_Sorted_genomes::const_iterator sg_i = sorted_genomes.begin();
        sg_i != sorted_genomes.end();
        ++sg_i) {
      genome_ranges.push_back(sg_i->second);
    }

    for(std::vector<std::vector<M_genome_range> >::const_iterator genome_range_i = genome_ranges.begin();
        genome_range_i != genome_ranges.end();
        ++genome_range_i) {
      find_adjacent_alignments(*genome_range_i, mat);
    }
  }
}


namespace Para_mugsy {

  Maf_alignment::Maf_alignment() :
    alignments_(), pos_()
  {}

  Maf_alignment::Maf_alignment(std::streampos const &pos) :
    pos_(pos)
  {}
  
  Maf_alignment::Maf_alignment(Maf_alignment const &ma) :
    alignments_(ma.alignments_), pos_(ma.pos_)
  {}

  Maf_alignment &Maf_alignment::operator=(Maf_alignment const &ma) {
    alignments_ = ma.alignments_;
    pos_ = ma.pos_;
    return *this;
  }

  void Maf_alignment::add_maf_entry(Maf_entry const &me) {
    for(std::vector<Maf_entry_alignment>::const_iterator i = me.alignments_begin();
        i != me.alignments_end();
        ++i) {
      alignments_.push_back(Maf_alignment_entry(i->range(), i->genome_name()));
    }
  }

  
  Maf_alignment_table alignment_table_of_ifstream(std::ifstream &in_stream) {
    Maf_read_stream maf_read_stream(in_stream);
    Maf_alignment_table ret;
    
    std::streampos pos = in_stream.tellg();
    while(M_option<Maf_entry> maf_entry_o = maf_read_stream.next()) {
      Maf_alignment ma(pos);
      ma.add_maf_entry(maf_entry_o.value());
      ret.push_back(ma);
      pos = in_stream.tellg();
    }

    return ret;
  }

  /*
   * maf_stitch_of_alignment_table takes a [Maf_alignment_table] and produces a collection
   * of alignment id's that can be stitched together to greate a single alignment.
   *
   * This works by taking the alignments table and, for each genome, creating a sorted
   * list of the alignment entries for that genome.  The alignments for each genome are
   * walked in adjacent pairs.  The entire genome should be present in the sorted list.
   * All of the alignments associated with that alignment id are compared to see if all
   * of them are adjacent.  If so then the alignment can be stitched together and it is
   * added to the list.
   * 
   */
  Maf_stitch maf_stitch_of_alignment_table(Maf_alignment_table const &maf_alignment_table) {
    Maf_stitch ret;

    Sorted_genomes sorted_genomes = sort_genomes(maf_alignment_table);

    print_adjacent_alignments(sorted_genomes, maf_alignment_table);
    return ret;
  }
}
