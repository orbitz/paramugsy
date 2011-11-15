#include <ifstream>
#include <vector>

#include <m_option.hh>
#include <maf_stitch.hh>

namespace Para_mugsy {
  Maf_alignemnt_table alignment_table_of_ifstream(std::ifstream &in_stream) {
    Maf_read_stream maf_read_stream(in_stream);
    Maf_alignment_table ret;
    
    int pos = in_stream.tellg();
    while(M_option<Maf_entry> maf_entry_o = maf_read_stream.next()) {
      ret.push_back(Maf_alignment_entry(maf_entry_o.value(), pos));
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

    
  }
}
