#ifndef MAF_READ_STREAM_HH
#define MAF_READ_STREAM_HH
#include <istream>
#include <sstream>
#include <vector>
#include <string>
#include <algorithm>
#include <exception>
#include <cassert>

#include <m_option.hh>
#include <m_direction.hh>
#include <m_range.hh>

namespace Para_mugsy {
  class Maf_parse_error : std::exception {};
  
  class Maf_entry_alignment {
  public:
    Maf_entry_alignment(std::string line) :
      /*
       * M_range<T> has no default ctor
       */
      range_(-1, -1)
    {
      std::istringstream iss(line);
      std::string dummy;
      std::string d;
      /* Get rid of start char */
      iss >> dummy;

      if(iss >>
         genome_name_ >>
         start_ >>
         size_ >>
         d >>
         src_size_ >>
         text_) {
        if("+" == d) {
          direction_ = D_FORWARD;
          range_ = of_maf(start_, size_, src_size_, direction_);
        }
        else {
          direction_ = D_REVERSE;
          range_ = of_maf(start_, size_, src_size_, direction_).reverse();
        }
      }
      else {
        throw Maf_parse_error();
      }
        
    }

    Maf_entry_alignment(Maf_entry_alignment const &mea) :
      genome_name_(mea.genome_name_),
      start_(mea.start_),
      size_(mea.size_),
      direction_(mea.direction_),
      src_size_(mea.src_size_),
      text_(mea.text_),
      range_(mea.range_)
    { }

    Maf_entry_alignment &operator=(Maf_entry_alignment const &mea) {
      Maf_entry_alignment copy(mea);
      swap(copy);
      return *this;
    }

    void swap(Maf_entry_alignment &rhs) {
      using std::swap;
      
      swap(genome_name_, rhs.genome_name_);
      swap(start_, rhs.start_);
      swap(size_, rhs.size_);
      swap(direction_, rhs.direction_);
      swap(src_size_, rhs.src_size_);
      swap(text_, rhs.text_);
      swap(range_, rhs.range_);
    }

    std::string const &genome_name() const {
      return genome_name_;
    }

    long start() const {
      return start_;
    }

    long size() const {
      return size_;
    }

    M_direction direction() const {
      return direction_;
    }

    long src_size() const {
      return src_size_;
    }

    std::string const &text() const {
      return text_;
    }

    M_range<long> const &range() const {
      return range_;
    }
    
  private:
    std::string genome_name_;
    long start_;
    long size_;
    M_direction direction_;
    long src_size_;
    std::string text_;
    M_range<long> range_;
  };
  
  class Maf_entry {
  public:
    Maf_entry(std::string const &score, std::string const &label) :
      score_(score), label_(label)
    {}
    
    std::string score() const {
      return score_;
    }

    std::string label() const {
      return label_;
    }

    void add_alignment(Maf_entry_alignment const &mea) {
      alignments_.push_back(mea);
    }

    std::vector<Maf_entry_alignment>::iterator alignments_begin() {
      return alignments_.begin();
    }

    std::vector<Maf_entry_alignment>::iterator alignments_end() {
      return alignments_.end();
    }

    std::vector<Maf_entry_alignment>::const_iterator alignments_begin() const {
      return alignments_.begin();
    }
    
    std::vector<Maf_entry_alignment>::const_iterator alignments_end() const {
      return alignments_.end();
    }
    
  private:
    std::string score_;
    std::string label_;
    std::vector<Maf_entry_alignment> alignments_;
  };
  
  class Maf_read_stream {
  public:
    Maf_read_stream(std::istream &istream) : istream_(istream) {}

    M_option<Maf_entry> next();
    
  private:
    std::istream &istream_;
  };

}

#endif
