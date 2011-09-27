#ifndef MAF_READ_STREAM_HH
#define MAF_READ_STREAM_HH

#include <istream>
#include <sstream>
#include <vector>
#include <string>
#include <algorithm>
#include <exception>

#include <m_option.hh>
#include <m_direction.hh>

namespace Para_mugsy {
  class Maf_parse_error : std::exception {};
  
  class Maf_entry_alignment {
  public:
    Maf_entry_alignment(std::string line) {
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
        }
        else {
          direction_ = D_REVERSE;
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
      text_(mea.text_)
    { }

    Maf_entry_alignment &operator=(Maf_entry_alignment const &mea) {
      Maf_entry_alignment copy(mea);
      swap(*this, copy);
      return *this;
    }

    void swap(Maf_entry_alignment &left, Maf_entry_alignment &right) {
      std::swap(left.genome_name_, right.genome_name_);
      std::swap(left.start_, right.start_);
      std::swap(left.size_, right.size_);
      std::swap(left.direction_, right.direction_);
      std::swap(left.src_size_, right.src_size_);
      std::swap(left.text_, right.text_);
    }
    
  private:
    std::string genome_name_;
    long start_;
    long size_;
    M_direction direction_;
    long src_size_;
    std::string text_;
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
