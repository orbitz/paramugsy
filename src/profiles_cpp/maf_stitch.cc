#include <iostream>

#include <vector>
#include <string>
#include <map>

#include <m_option.hh>
#include <maf_stitch.hh>


namespace {
  using namespace Para_mugsy;

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

    for(Sorted_genomes::iterator i = ret.begin();
        i != ret.end();
        ++i) {
      std::sort(i->second.begin(), i->second.end(), genome_range_sort_comparator);
    }

    return ret;
  }


  Maf_sequence_alignment stitch_alignment_singleton(Maf_genome_range const &gr) {
    return Maf_sequence_alignment(1, Maf_alignment_point(1, gr.alignment_id()));
  }

  Maf_stitch stitch_alignments(Sorted_genomes sorted_genomes, Maf_alignment_table const &mat) {
    Maf_stitch stitched;

    while(!sorted_genomes.empty()) {
      Sorted_genomes::iterator seed_genome = sorted_genomes.begin();

      if(seed_genome->second.size() > 1) {
        M_genome_range const &seed = *seed_genome->second.begin();
        Maf_alignment_id align_id = seed.alignment_id();



      }
      else {
        stitched.append(stitch_alignment_singleton(*seed_genome->second.begin()));
      }

      seed_genome->second.erase(seed_genome->second.begin());
      if(seed_genome->second.empty()) {
        sorted_genomes.erase(seed_genome->first);
      }
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
    return stitch_alignments(sort_genomes(maf_alignment_table), maf_alignment_table);
  }
}
