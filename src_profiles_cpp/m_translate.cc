#include <iostream>

#include <fstream>
#include <ostream>

#include <vector>
#include <string>
#include <map>

#include <cassert>

#include <m_delta.hh>
#include <m_delta_builder.hh>
#include <m_profile.hh>
#include <m_metaprofile.hh>
#include <m_translate.hh>

#include <m_fileutils.hh>

using namespace Para_mugsy;

namespace {
  class Already_unnext_gap : public std::exception {};

  std::vector<long> _deltas_of_gaps(M_delta_entry const& de);
  
  class _delta_stream_writer {
  public:
    _delta_stream_writer(std::ostream& out_stream) :
      out_stream(out_stream)
    {}

    void write(M_delta_entry const& de) {
      if(de.header_names != header_names) {
        out_stream << '>' << de.header_names.first << ' ' << de.header_names.second << ' ';
        header_names = de.header_names;
      }
      
      std::vector<long> gaps = _deltas_of_gaps(de);
      out_stream << de.header_lengths.first << ' ' << de.header_lengths.second << '\n';
      out_stream << de.ref_range.get_start() << ' ' << de.ref_range.get_end() << ' ';
      out_stream << de.query_range.get_start() << ' ' << de.query_range.get_end() << " 1 2 3\n";
      for(std::vector<long>::const_iterator i = gaps.begin();
          i != gaps.end();
          ++i) {
        out_stream << *i << '\n';
      }
    }
    
  private:
    std::ostream& out_stream;
    std::pair<std::string, std::string> header_names;
  };

  
  class _gap_iterator {
  public:
    _gap_iterator(std::vector<M_range<M_profile_idx> >::const_iterator ref_start,
                  std::vector<M_range<M_profile_idx> >::const_iterator ref_end,
                  std::vector<M_range<M_profile_idx> >::const_iterator query_start,
                  std::vector<M_range<M_profile_idx> >::const_iterator query_end):
      ref_curr(ref_start), ref_end(ref_end), query_curr(query_start), query_end(query_end),
      ref_tmp_gap(0, 0), ref_tmp_gap_ptr(0), query_tmp_gap(0, 0), query_tmp_gap_ptr(0)
    {}

    M_option<std::pair<strand_t, M_range<M_profile_idx> > > curr(M_profile_idx ref_s, M_profile_idx query_s) const {
      if((ref_tmp_gap_ptr || ref_curr != ref_end) && (query_tmp_gap_ptr || query_curr != query_end)) {
        M_range<M_profile_idx> const& ref_gap = ref_tmp_gap_ptr ? *ref_tmp_gap_ptr : *ref_curr;
        M_range<M_profile_idx> const& query_gap = query_tmp_gap_ptr ? *query_tmp_gap_ptr : *query_curr;

        long r_diff = ref_gap.get_start() - ref_s;
        long q_diff = query_gap.get_start() - query_s;

        //std::cout << "r_diff = " << r_diff << std::endl;
        //std::cout << "q_diff = " << q_diff << std::endl;
        //std::cout << "ref_s = " << ref_s << " query_s = " << query_s << std::endl;
        //std::cout << "ref_gap = (" << ref_gap.get_start() << ", " << ref_gap.get_end() << ")\n";
        //std::cout << "query_gap = (" << query_gap.get_start() << ", " << query_gap.get_end() << ")\n";
        
        assert(r_diff >= 0);
        assert(q_diff >= 0);
        if(r_diff <= q_diff) {
          return M_option<std::pair<strand_t, M_range<M_profile_idx> > >(std::make_pair(S_REF, ref_gap));
        }
        else {
          return M_option<std::pair<strand_t, M_range<M_profile_idx> > >(std::make_pair(S_QUERY, query_gap));
        }        
      }
      else if(ref_tmp_gap_ptr || ref_curr != ref_end) {
        M_range<M_profile_idx> const& tmp = ref_tmp_gap_ptr ? *ref_tmp_gap_ptr : *ref_curr;
        return M_option<std::pair<strand_t, M_range<M_profile_idx> > >(std::make_pair(S_REF, tmp));
      }
      else if(query_tmp_gap_ptr || query_curr != query_end) {
        M_range<M_profile_idx> const& tmp = query_tmp_gap_ptr ? *query_tmp_gap_ptr : *query_curr;
        return M_option<std::pair<strand_t, M_range<M_profile_idx> > >(std::make_pair(S_QUERY, tmp));
      }
      else {
        return M_option<std::pair<strand_t, M_range<M_profile_idx> > >();
      }
    }

    void unnext(strand_t s, M_range<M_profile_idx> const& gap) {
      if(S_REF == s && !ref_tmp_gap_ptr) {
        ref_tmp_gap = gap;
        ref_tmp_gap_ptr = &ref_tmp_gap;
      }
      else if(S_QUERY == s && !query_tmp_gap_ptr) {
        query_tmp_gap = gap;
        query_tmp_gap_ptr = &query_tmp_gap;
      }      
      else {
        throw Already_unnext_gap();
      }
    }

    M_option<M_range<M_profile_idx> > peek(strand_t s) const {
      if(S_REF == s && ref_tmp_gap_ptr) {
        return M_option<M_range<M_profile_idx> >(*ref_tmp_gap_ptr);
      }
      else if(S_REF == s && ref_curr != ref_end) {
        return M_option<M_range<M_profile_idx> >(*ref_curr);
      }
      else if(S_QUERY == s && query_tmp_gap_ptr) {
        return M_option<M_range<M_profile_idx> >(*query_tmp_gap_ptr);
      }
      else if(S_QUERY == s && query_curr != query_end) {
        return M_option<M_range<M_profile_idx> >(*query_curr);
      }
      else {
        return M_option<M_range<M_profile_idx> >();
      }
    }

    bool at_end() const {
      return !ref_tmp_gap_ptr && ref_curr == ref_end && !query_tmp_gap_ptr && query_curr == query_end;
    }
    
    void advance_strand(strand_t s) {
      if(S_REF == s) {
        advance_ref_iter();
      }
      else {
        advance_query_iter();
      }
    }
    
  private:
    void advance_ref_iter() {
      if(ref_tmp_gap_ptr) {
        ref_tmp_gap_ptr = 0;
      }
      else {
        ++ref_curr;
      }
    }
    
    void advance_query_iter() {
      if(query_tmp_gap_ptr) {
        query_tmp_gap_ptr = 0;
      }
      else {
        ++query_curr;
      }
    }
    
    std::vector<M_range<M_profile_idx> >::const_iterator ref_curr;
    std::vector<M_range<M_profile_idx> >::const_iterator ref_end;
    std::vector<M_range<M_profile_idx> >::const_iterator query_curr;
    std::vector<M_range<M_profile_idx> >::const_iterator query_end;

    M_range<M_profile_idx> ref_tmp_gap;
    M_range<M_profile_idx> *ref_tmp_gap_ptr;

    M_range<M_profile_idx> query_tmp_gap;
    M_range<M_profile_idx> *query_tmp_gap_ptr;    
    
  };

  struct _gd_state {
    _gd_state(M_profile const& ref_profile,
              M_metaprofile const& query_metaprofile,
              _gap_iterator& profile_gaps,
              _gap_iterator& d_profile_gaps,
              M_profile_idx ref_profile_pos,
              M_profile_idx query_metaprofile_pos,
              M_profile_idx d_profile_pos,
              M_profile_idx d_profile_end) :
      ref_profile(ref_profile),
      query_metaprofile(query_metaprofile),
      profile_gaps(profile_gaps),
      d_profile_gaps(d_profile_gaps),
      ref_profile_pos(ref_profile_pos),
      query_metaprofile_pos(query_metaprofile_pos),
      d_profile_pos(d_profile_pos),
      d_profile_end(d_profile_end)
    {}

    M_profile const& ref_profile;
    M_metaprofile const& query_metaprofile;
    _gap_iterator& profile_gaps;
    _gap_iterator& d_profile_gaps;
    M_profile_idx ref_profile_pos;
    M_profile_idx query_metaprofile_pos;
    M_profile_idx d_profile_pos;
    M_profile_idx d_profile_end;
  };
  

  bool _is_not_idx(std::string const& f) {
    std::string::size_type s = f.find(".idx");
    return s != std::string::npos;
  }
  
  std::vector<std::string> _list_dir_idx(std::string const& dir) {
    std::vector<std::string> ret = list_dir(dir);
    std::vector<std::string>::iterator new_last = remove_if(ret.begin(),
                                                            ret.end(),
                                                            _is_not_idx);
    ret.erase(ret.begin(), new_last);

    return ret;
  }

  std::map<std::string, std::vector<M_profile> > _profile_map_of_dir(std::string const& dir) {
    std::vector<std::string> files = _list_dir_idx(dir);
    std::map<std::string, std::vector<M_profile> > ret;

    for(std::vector<std::string>::const_iterator i = files.begin();
        i != files.end();
        ++i) {
      M_profile profile = read_profile_file(true, *i);
      ret[profile.p_seq_name].push_back(profile);
    }
    return ret;
  }


  M_delta_entry _reverse_if_needed(M_delta_entry const& de, M_profile const& left_profile) {
    if(de.ref_range.get_direction() != left_profile.p_range.get_direction()) {
      return de.reverse();
    }
    else {
      return de;
    }
  }

  void _push_ones(std::vector<long>& gaps, strand_t s, long ones) {
    long one = S_REF == s ? -1 : 1;
    for(; ones > 0; --ones) {
      gaps.push_back(one);
    }
  }
  
  std::vector<long> _deltas_of_gaps(M_delta_entry const& de) {
    std::vector<long> ret;
    std::vector<M_range<M_profile_idx> >::const_iterator ref_i = de.ref_gaps.begin();
    std::vector<M_range<M_profile_idx> >::const_iterator query_i = de.query_gaps.begin();
    long pos = 0;
    
    while(1) {
      if(ref_i != de.ref_gaps.end() && query_i != de.query_gaps.end()) {
        if(ref_i->get_start() < query_i->get_start()) {
          ret.push_back(-(ref_i->get_start() - pos));
          _push_ones(ret, S_REF, ref_i->length() - 1);
          pos = ref_i->get_end();
          ++ref_i;
        }
        else {
          ret.push_back(-(query_i->get_start() - pos));
          _push_ones(ret, S_QUERY, query_i->length() - 1);
          pos = query_i->get_end();
          ++query_i;
        }
      }
      else if(ref_i != de.ref_gaps.end()) {
        ret.push_back(-(ref_i->get_start() - pos));
        _push_ones(ret, S_REF, ref_i->length() - 1);
        pos = ref_i->get_end();
        ++ref_i;
      }
      else if(query_i != de.query_gaps.end()) {
        ret.push_back(-(query_i->get_start() - pos));
        _push_ones(ret, S_QUERY, query_i->length() - 1);
        pos = query_i->get_end();
        ++query_i;
      }
      else {
        ret.push_back(0);
        break;
      }
    }
    return ret;
  }

  void _update_pos_by_d_gap_strand(strand_t s, M_range<long> const& d_diff, _gd_state& gd) {
    if(S_REF == s) {
      gd.ref_profile_pos += d_diff.get_start();
      gd.query_metaprofile_pos += d_diff.get_end() + 1;
      gd.d_profile_pos += d_diff.get_end() + 1;
    }
    else {
      gd.ref_profile_pos += d_diff.get_end() + 1;
      gd.query_metaprofile_pos += d_diff.get_start();
      gd.d_profile_pos += d_diff.get_end() + 1;
    }
  }
  
  void _update_pos_by_p_gap_strand(strand_t s, M_range<long> const& gr_diff, _gd_state& gd) {
    if(S_REF == s) {
      gd.ref_profile_pos += gr_diff.get_end() + 1;
      gd.query_metaprofile_pos += gr_diff.get_start();
      gd.d_profile_pos += gr_diff.get_start();
    }
    else {
      gd.ref_profile_pos += gr_diff.get_start();
      gd.query_metaprofile_pos += gr_diff.get_end() + 1;
      gd.d_profile_pos += gr_diff.get_start();
    }
  }

  bool _overlap_opposite_strand(strand_t s, M_range<long> d_diff, _gap_iterator const& profile_gaps, _gd_state const& gd) {
    if(S_REF == s) {
      if(M_option<M_range<M_profile_idx> > gap_o = profile_gaps.peek(S_QUERY)) {
        long diff = gap_o.value().get_start() - gd.query_metaprofile_pos;
        return diff <= d_diff.get_end();
      }
      else {
        return false;
      }
    }
    else if(S_QUERY == s) {
      if(M_option<M_range<M_profile_idx> > gap_o = profile_gaps.peek(S_REF)) {
        long diff = gap_o.value().get_start() - gd.ref_profile_pos;
        return diff <= d_diff.get_end();
      }
      else {
        return false;
      }
    }
    else {
      return false;
    }
  }

  M_option<M_range<M_profile_idx> > _get_gap_opposite_strand(strand_t s, _gap_iterator const& profile_gaps) {
    if(S_REF == s) {
      return profile_gaps.peek(S_QUERY);
    }
    else {
      return profile_gaps.peek(S_REF);
    }
  }
  
  void _generate_delta_alignment(_gd_state& gd, M_delta_builder& db, _delta_stream_writer& dsw) {
    M_option<std::pair<strand_t, M_range<M_profile_idx> > > gr_gap_o = gd.profile_gaps.curr(gd.ref_profile_pos, gd.query_metaprofile_pos);
    M_option<std::pair<strand_t, M_range<M_profile_idx> > > d_gap_o = gd.d_profile_gaps.curr(gd.d_profile_pos, gd.d_profile_pos);
    
    if(gr_gap_o && d_gap_o) {
      std::pair<strand_t, M_range<M_profile_idx> > const& gr_gap = gr_gap_o.value();
      std::pair<strand_t, M_range<M_profile_idx> > const& d_gap = d_gap_o.value();

      M_range<long> gr_diff(0, 0);
      if(S_REF == gr_gap.first) {
        gr_diff = M_range<long>(gr_gap.second.get_start() - gd.ref_profile_pos,
                                gr_gap.second.get_end() - gd.ref_profile_pos);
      }
      else {
        gr_diff = M_range<long>(gr_gap.second.get_start() - gd.query_metaprofile_pos,
                                gr_gap.second.get_end() - gd.query_metaprofile_pos);
      }

      M_range<long> d_diff(d_gap.second.get_start() - gd.d_profile_pos,
                           d_gap.second.get_end() - gd.d_profile_pos);

      /*
       * There are 2 situations here.  
       * 1 - One is that our gap starts before or at the same offset as
       *     the d_gap.
       * 2 - The d_gap comes before the gap, in that case we need to determine if the d_gap
       *     overlaps with the gap in any way, if so, generate an alignment and cut up the d_gap
       *     into the part we can handle and the part we need to put off until later.
       */
      if(gr_diff.get_start() <= d_diff.get_start()) {
        db.add_offset(gr_diff.get_start());
        _update_pos_by_p_gap_strand(gr_gap.first, gr_diff, gd);
        gd.profile_gaps.advance_strand(gr_gap.first);
        M_option<M_delta_entry> de_o = db.to_delta();
        db.reset(gd.ref_profile_pos, gd.query_metaprofile_pos);
        //std::cout << "1\n";
        if(de_o) {
          dsw.write(de_o.value());
        }
      }
      else if(d_diff.get_end() < gr_diff.get_start() || (gr_gap.first == d_gap.first &&
                                                         !_overlap_opposite_strand(gr_gap.first, d_diff, gd.profile_gaps, gd))) {
        db.add_gap(d_gap.first, d_diff);
        _update_pos_by_d_gap_strand(d_gap.first, d_diff, gd);
        gd.d_profile_gaps.advance_strand(d_gap.first);
        //std::cout << "2\n";
      }
      else if(gr_gap.first == d_gap.first) {
        /*
	 * In this case the gr gap does not start infront of the d gap
	 * and the d gap does not finish before the gr gap.  We have some overlap
	 * between the gr gap and the d gap.  `overlap_opposite_strand` checks to see if
	 * our d gap is overlapping the gr gap on the opposite strand. If d_strand equals 
	 * strand then we know that the d gap does not end before the gr gap and that 
	 * we overlap the opposite strands gap. 
	 * We need to know this because when we add the d gap, we increment the profile 
	 * position on the strand opposite of the current gr gap by the total length of 
	 * the d gap.  Consider the situation below:
	 * 
	 * 
	 *     r |------XX-----------|
	 *     q |----------XX-------|
	 *    dr |----XXXXXXXXXX-----|
	 *    dq |-------------------|
	 * 
	 * In this case `strand` is r and `d_strand` is r.  If we blindly add the d gap, the following
	 * positions would look like:
	 * 
	 * 
	 *     r |------XX-----------|
	 *            ^
	 *     q |----------XX-------|
	 *                      ^
	 *    dr |----XXXXXXXXXX-----|
	 *                      ^
	 *    dq |-------------------|
	 *                      ^
	 * 
	 * We have steped over the gap on the q strand.  Instead what we will do is check to see if the current
	 * d gap overlaps the next gap on the opposing strand and, if so, break our d gap into 2 gaps.  At the
	 * end of this function we should look like:
	 * 
	 *     r |------XX-----------|
	 *            ^
	 *     q |----------XX-------|
	 *                  ^
	 *    dr |----XXXXXXXXXX-----|
	 *                  ^
	 *    dq |-------------------|
	 *                  ^
         */
        if(M_option<M_range<M_profile_idx> > gr_opp_gap_o = _get_gap_opposite_strand(gr_gap.first, gd.profile_gaps)) {
          M_range<long> gr_opp_diff(0, 0);
          if(S_REF == gr_gap.first) {
            gr_opp_diff = M_range<long>(gr_opp_gap_o.value().get_start() - gd.query_metaprofile_pos,
                                        gr_opp_gap_o.value().get_end() - gd.query_metaprofile_pos);
          }
          else {
            gr_opp_diff = M_range<long>(gr_opp_gap_o.value().get_start() - gd.ref_profile_pos,
                                        gr_opp_gap_o.value().get_end() - gd.ref_profile_pos);
          }

          long diff_to_opp_gap = gr_opp_diff.get_start() - d_diff.get_start();
          M_range<M_profile_idx> d_gr_diff_split(d_diff.get_start(), d_diff.get_start() + diff_to_opp_gap - 1);
          M_range<M_profile_idx> d_gr_back(d_gap.second.get_start() + diff_to_opp_gap, d_gap.second.get_end());
          db.add_gap(d_gap.first, d_gr_diff_split);
          _update_pos_by_d_gap_strand(d_gap.first, d_gr_diff_split, gd);
          gd.d_profile_gaps.advance_strand(d_gap.first);
          gd.d_profile_gaps.unnext(d_gap.first, d_gr_back);
          //std::cout << "3\n";
        }
      }
      else {
        /*
         * The gap isn't before d_gap and d_gap isn't entirely before gap and the
         * gaps aren't on the same strand.  This means that the d_gap overlaps
         * the gap.  In this case we have to break up the d_gap, put part of it
         * back into its gap list and then process what we can
         */
        long diff_to_gap = gr_diff.get_start() - d_diff.get_start();
        M_range<M_profile_idx> d_gr_diff_split(d_diff.get_start(), d_diff.get_start() + diff_to_gap - 1);
        db.add_gap(d_gap.first, d_gr_diff_split);
        M_range<M_profile_idx> d_gr_back(d_gap.second.get_start() + diff_to_gap, d_gap.second.get_end());
        _update_pos_by_d_gap_strand(d_gap.first, d_gr_diff_split, gd);
        gd.d_profile_gaps.advance_strand(d_gap.first);
        gd.d_profile_gaps.unnext(d_gap.first, d_gr_back);
        //std::cout << "4\n";
      }
    }
    else if(gr_gap_o) {
      std::pair<strand_t, M_range<M_profile_idx> > const& gr_gap = gr_gap_o.value();
      /*
       * We just have gaps left in the profiles we are producing alignments for.  In this case we have to
       * produce an alignment under some conditions.  Conditions where we do not want to produce
       * an alignment include:
       * 
       * 1 - We are at the start of a new alignment and the first thing we hit is a gap.  In this case
       *     we just want to advance on to creating the next alignment.
       * 2 - We have a situation where a d_gap spans multiple gaps.  For example:
       * 
       *     r |-----XX----XX------|
       *     q |-------------------|
       *    dr |----XXXXXXXXXX-----|
       *    dq |-------------------|
       * 
       *     In this case, the portion in r between the twp gaps is not actually a valid alignment because 
       *     the other strand is nothing but gaps.  That means we want to throw this alignment away
       *     as well and continue on to the next one.
       * 
       * Both of these situations are identifiable through the same method: the distance to the gap will
       * be 0 and one of the strands in the delta will have not moved at all.
       */
      M_range<long> gr_diff(0, 0);
      if(S_REF == gr_gap.first) {
        gr_diff = M_range<long>(gr_gap.second.get_start() - gd.ref_profile_pos,
                                gr_gap.second.get_end() - gd.ref_profile_pos);
      }
      else {
        gr_diff = M_range<long>(gr_gap.second.get_start() - gd.query_metaprofile_pos,
                                gr_gap.second.get_end() - gd.query_metaprofile_pos);
      }
      db.add_offset(gr_diff.get_start());
      _update_pos_by_p_gap_strand(gr_gap.first, gr_diff, gd);
      gd.profile_gaps.advance_strand(gr_gap.first);
      M_option<M_delta_entry> de = db.to_delta();
      db.reset(gd.ref_profile_pos, gd.query_metaprofile_pos);
      //std::cout << "5\n";
      if(de) {
        dsw.write(de.value());
      }
    }
    else if(d_gap_o) {
      std::pair<strand_t, M_range<M_profile_idx> > const& d_gap = d_gap_o.value();
      M_range<long> d_diff(d_gap.second.get_start() - gd.d_profile_pos,
                           d_gap.second.get_end() - gd.d_profile_pos);

      db.add_gap(d_gap.first, d_diff);
      _update_pos_by_d_gap_strand(d_gap.first, d_diff, gd);
      gd.d_profile_gaps.advance_strand(d_gap.first);
      //std::cout << "6\n";
    }
    else {
      /*
       * If there are no gaps on either side then the entire block can become an alignment
       * We only want to generate an alignment if our current position is not at the end
       * or if it is at the end we know that the start in the skeleton isn't where we are.
       * This is because it could be that we just had a gap go all the way to the end
       *
       * I'm not sure this check here is actually correct, need to add some checking
       * to see how we reach a state where d_profile_pos is greater than d_profile_end
       */
      if(gd.d_profile_pos <= gd.d_profile_end) {
        long diff = gd.d_profile_end - gd.d_profile_pos + 1;
        db.add_offset(diff);
        //std::cout << "7\n";
        if(M_option<M_delta_entry> de = db.to_delta()) {
          dsw.write(de.value());
        }
      }
    }
  }
  
  void _generate_delta(M_delta_entry const& de,
                       M_profile const& ref_profile,
                       M_range<M_seq_idx> const& ref_seq,
                       M_profile const& query_profile,
                       M_range<M_seq_idx> const& query_seq,
                       _delta_stream_writer& dsw) {
    /*
     * We know that both profiles overlap our delta entry in some way but we do not know if
     * where they overlap the delta entry, the delta entry still overlaps itself.  That is to say
     * if I have a delta entry with the reference and query range of 0 - 10, what if my left
     * profile overlaps the reference range at 0 - 3 and my right profile overlaps the query range
     * at 6 - 10.  This combination of values no longer overlap each other in which case we have
     * no work to do.  On the other hand they overlaped at 4 - 8 and 6 - 10 we would have an overlap of
     * 6 - 8 that needed to be translated into the new delta.
     *
     * Our first step is to take the delta entry and create profiles of its reference and query sections.
     * `ref_seq` and `query_seq` are defined in terms of the original genome sequences so we convert those
     * to profile.  The ref and query profiles from delta entry are the same length so a point that is on
     * one will correspond to the same location on the other (when using profile indecies).  That means
     * with the profile indecies that `ref_seq` and `query_seq` have been mapped back to we can see
     * if they overlap.  If they do, we have a region to convert. 
     */
    M_profile d_ref_profile("",
                            "",
                            "",
                            de.ref_range,
                            de.ref_gaps);
    
    M_profile d_query_profile("",
                              "",
                              "",
                              de.query_range,
                              de.query_gaps);

    M_range<M_profile_idx> d_ref_range(profile_idx_of_seq_idx(d_ref_profile, ref_seq.get_start()),
                                       profile_idx_of_seq_idx(d_ref_profile, ref_seq.get_end()));
    M_range<M_profile_idx> d_query_range(profile_idx_of_seq_idx(d_query_profile, query_seq.get_start()),
                                         profile_idx_of_seq_idx(d_query_profile, query_seq.get_end()));

    if(M_option<M_range<M_profile_idx> > d_overlap = overlap(d_ref_range, d_query_range)) {
      /*
       * The location that the left and right profiles overlap the delta entry still allows the
       * reference and query in the delta entry to overlap.
       *
       * Now we need to translate the section, but first we need to know what we are translating. We need
       * two peices of information per profile: the starting location and the gaps for the range we are
       * translating.  We finally need the ending location for just one profile since it will correspond
       * to the ending location for all of them.
       *
       * There is one condition that will keep us from translating anything here: if the new area is all gaps
       * in the left or right profile.
       */
      
      M_option<M_profile> d_ref_profile_sub_o = subset_profile(d_ref_profile,
                                                               d_overlap.value().get_start(),
                                                               d_overlap.value().get_end());

      M_option<M_profile> d_query_profile_sub_o = subset_profile(d_query_profile,
                                                                 d_overlap.value().get_start(),
                                                                 d_overlap.value().get_end());

      if(d_ref_profile_sub_o && d_query_profile_sub_o) {
        M_profile const& d_ref_profile_sub = d_ref_profile_sub_o.value();
        M_profile const& d_query_profile_sub = d_query_profile_sub_o.value();

        M_profile ref_profile_sub = subset_seq(ref_profile,
                                               d_ref_profile_sub.p_range.get_start(),
                                               d_ref_profile_sub.p_range.get_end());

        M_profile query_profile_sub = subset_seq(query_profile,
                                                 d_query_profile_sub.p_range.get_start(),
                                                 d_query_profile_sub.p_range.get_end());


        /*
         * The sub profiles for delta and profiles should be equal for the respective types
         */
        assert(d_ref_profile_sub.p_range.length() == ref_profile_sub.p_range.length() && d_query_profile_sub.p_range.length() == query_profile_sub.p_range.length());

        /*
         * Setup our query metaprofile and reverse the gaps if we'll actually be walking the
         * profile in reverse
         */
        bool reverse_metaprofile = query_profile.p_range.get_direction() != d_query_profile.p_range.get_direction();
        M_metaprofile query_metaprofile(query_profile, reverse_metaprofile);
        std::vector<M_range<M_profile_idx> > query_profile_gaps;
        if(query_metaprofile.is_reversed()) {
          for(std::vector<M_range<M_profile_idx> >::reverse_iterator i = query_profile_sub.p_gaps.rbegin();
              i != query_profile_sub.p_gaps.rend();
              ++i) {
            //std::cout << "reversing\n";
            query_profile_gaps.push_back(M_range<M_profile_idx>(query_metaprofile.profile_idx_of_profile_idx(i->get_end()),
                                                                query_metaprofile.profile_idx_of_profile_idx(i->get_start())));
          }
        }
        else {
          query_profile_gaps = query_profile_sub.p_gaps;
        }

        M_profile_idx ref_start = profile_idx_of_seq_idx(ref_profile, ref_profile_sub.p_range.get_start());
        M_profile_idx query_start;

        if(query_metaprofile.is_reversed()) {
          query_start =
            query_metaprofile.profile_idx_of_profile_idx(profile_idx_of_seq_idx(query_profile, query_profile_sub.p_range.get_end()));
        }
        else {
          query_start = profile_idx_of_seq_idx(query_profile, query_profile_sub.p_range.get_start());
        }

        //std::cout << "query_profile_sub.p_range = (" << query_profile_sub.p_range.get_start() << ", " << query_profile_sub.p_range.get_end() << ")\n";

        M_profile_idx d_profile_pos = d_overlap.value().get_start();
        M_profile_idx d_profile_end = d_overlap.value().get_end();

        _gap_iterator profile_gaps(ref_profile_sub.p_gaps.begin(),
                                   ref_profile_sub.p_gaps.end(),
                                   query_profile_gaps.begin(),
                                   query_profile_gaps.end());
        _gap_iterator d_profile_gaps(d_ref_profile_sub.p_gaps.begin(),
                                     d_ref_profile_sub.p_gaps.end(),
                                     d_query_profile_sub.p_gaps.begin(),
                                     d_query_profile_sub.p_gaps.end());
        
        _gd_state gd(ref_profile,
                     query_metaprofile,
                     profile_gaps,
                     d_profile_gaps,
                     ref_start,
                     query_start,
                     d_profile_pos,
                     d_profile_end);
        
        M_delta_builder db(std::make_pair("", ""),
                           std::make_pair(ref_profile.p_major_name, ref_profile.p_length),
                           std::make_pair(query_profile.p_major_name, query_profile.p_length),
                           ref_start,
                           query_start,
                           query_metaprofile);

        //std::cout << ref_profile << "\n";
        //std::cout << ref_profile_sub << "\n";
        //std::cout << query_profile << "\n";
        //std::cout << query_profile_sub << "\n";
        //for(std::vector<M_range<M_profile_idx> >::const_iterator i = query_profile_gaps.begin();
        //    i != query_profile_gaps.end();
        //    ++i) {
        //  std::cout << "(" << i->get_start() << ", " << i->get_end() << ")";
        //}
        //std::cout << "\n";
        //std::cout << d_ref_profile_sub << "\n";
        //std::cout << d_query_profile_sub << "\n";
        //std::cout << "ref_start = " << ref_start << "\n";
        //std::cout << "query_start = " << query_start << "\n";
        //std::cout << "d_profile_pos = " << d_profile_pos << "\n";
        
        //std::cout << "Looping...\n";
        while(!gd.profile_gaps.at_end() && !gd.profile_gaps.at_end()) {
          _generate_delta_alignment(gd, db, dsw);
        }
        /*
         * And we do one more to catch the ending set
         */
        //std::cout << "Final call...\n";
        _generate_delta_alignment(gd, db, dsw);
      }
    }
  }
    

  
  void _translate_delta_with_profiles(M_profile const& left_profile,
                                      M_profile const& right_profile,
                                      M_delta_entry const& de,
                                      _delta_stream_writer& dsw) {
    /*
     * We know that the profiles correspond to the names in the delta entry but
     * we don't know if these profiles actually overlap the delta entry at all.
     * If both profiles overlap our delta entry at all then we need to translate
     * the overlapped area back into the context of the delta sequences.  With
     * these values we call _generate_delta.
     */
    M_option<M_range<M_seq_idx> > ref_overlap_o = overlap(de.ref_range, left_profile.p_range);
    M_option<M_range<M_seq_idx> > query_overlap_o = overlap(de.query_range, right_profile.p_range);
    
    if(ref_overlap_o && query_overlap_o) {
      M_range<M_seq_idx> const& ref_overlap = ref_overlap_o.value();
      M_range<M_seq_idx> const& query_overlap = query_overlap_o.value();

      M_delta_entry n_de = _reverse_if_needed(de, left_profile);

      _generate_delta(n_de, left_profile, ref_overlap, right_profile, query_overlap, dsw);
    }
  }
  
  
  void _translate_delta(std::map<std::string, std::vector<M_profile> > const& left_profile_map,
                        std::map<std::string, std::vector<M_profile> > const& right_profile_map,
                        M_delta_stream& ds,
                        _delta_stream_writer& dsw) {
    while(M_option<M_delta_entry> d = ds.next()) {
      std::map<std::string, std::vector<M_profile> >::const_iterator left_profiles_i = left_profile_map.find(d.value().header_names.first);
      std::map<std::string, std::vector<M_profile> >::const_iterator right_profiles_i = right_profile_map.find(d.value().header_names.second);      

      if(left_profiles_i != left_profile_map.end() && right_profiles_i != right_profile_map.end()) {
        std::vector<M_profile> const& left_profiles = left_profiles_i->second;
        std::vector<M_profile> const& right_profiles = right_profiles_i->second;

        /*
         * For every combinatino of left and right profile that are for our genome
         * we call try to translate any overlapping area between the profiles and
         * the delta.
         *
         * We don't know which of the profile combinations will overlap,
         * _translate_delta_profiles will determine that, here we are simply
         * calling for every combination.  It is guaranteed that at least one
         * combination of profiles will overlap the delta entry but we don't know
         * which.  Some kind of interval tree might be a useful optimization here.
         */
        for(std::vector<M_profile>::const_iterator left_i = left_profiles.begin();
            left_i != left_profiles.end();
            ++left_i) {
          for(std::vector<M_profile>::const_iterator right_i = right_profiles.begin();
              right_i != right_profiles.end();
              ++right_i) {
            _translate_delta_with_profiles(*left_i, *right_i, d.value(), dsw);
          }
        }
      }
    }
  }
}

namespace Para_mugsy {
  
  void translate(std::string const& left_dir,
                 std::string const& right_dir,
                 std::vector<std::string> const& nucmer_list,
                 std::ostream& out_stream) {
    std::map<std::string, std::vector<M_profile> > left_profile_map = _profile_map_of_dir(left_dir);
    std::map<std::string, std::vector<M_profile> > right_profile_map = _profile_map_of_dir(right_dir);

    _delta_stream_writer dsw(out_stream);
    
    for(std::vector<std::string>::const_iterator i = nucmer_list.begin();
        i != nucmer_list.end();
        ++i) {
      std::ifstream in_delta_stream(i->c_str());
      M_delta_stream ds(in_delta_stream);
      _translate_delta(left_profile_map, right_profile_map, ds, dsw);
    }
  }

  
}
