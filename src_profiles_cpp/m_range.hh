#ifndef M_RANGE_HH
#define M_RANGE_HH

#include <algorithm>

#include <m_option.hh>
#include <m_direction.hh>

namespace Para_mugsy {
  
  template <typename T>
  class M_range {
  public:
    M_range(T const &s, T const& e) : start(s), end(e) {}
    M_range(M_range<T> const &range) : start(range.start), end(range.end) {}

    M_range<T> &operator=(M_range<T> const& rhs) {
      M_range<T> copy(rhs);
      swap(copy);
      return *this;
    }

    void swap(M_range<T> &rhs) {
      using std::swap;

      swap(start, rhs.start);
      swap(end, rhs.end);
    }
      
    
    T get_start() const { return start; }
    T get_end() const { return end; }
    
    unsigned long length() const { return (start <= end ? end - start : start - end) + 1; }
    
    M_direction get_direction() const { return start <= end ? D_FORWARD : D_REVERSE; }

    M_range<T> reverse() const { return M_range(end, start); }

    bool contains(T const& v) const {
      M_range<T> r = make_forward(*this);
      return r.get_start() <= v && v <= r.get_end();
    }
    
  private:
    T start;
    T end;
    
  };

  template <typename T>
  void swap(M_range<T> &lhs, M_range<T> &rhs) {
    lhs.swap(rhs);
  }

  template <typename T>
  M_range<T> make_forward(M_range<T> const& r) {
    switch(r.get_direction()) {
    case D_FORWARD:
      return r;
    case D_REVERSE:
      return r.reverse();
    }

    /*
     * Should never reach this, shutting up compiler thouhg
     */
    throw std::exception();
  }
  
  template <typename T>
  M_option<M_range<T> > overlap(M_range<T> const& range_1, M_range<T> const& range_2) {
    M_range<T> r_1 = make_forward(range_1);
    M_range<T> r_2 = make_forward(range_2);

    T s = std::max(r_1.get_start(), r_2.get_start());
    T e = std::min(r_1.get_end(), r_2.get_end());

    if((e - s) >= 0) {
      return M_option<M_range<T> >(M_range<T>(s, e));
    }
    else {
      return M_option<M_range<T> >();
    }
  }

  template <typename T>
  M_range<T> of_pair(std::pair<T, T> const& p) {
    return M_range<T>(p.first, p.second);
  }

  template <typename T>
  std::pair<T, T> to_pair(M_range<T> const& r) {
    return std::make_pair(r.get_start(), r.get_end());
  }

  inline M_range<long> of_maf(long start, long size, long src_size, M_direction direction) {
    switch(direction) {
    case D_FORWARD:
      return M_range<long>(start + 1, start + size);
    case D_REVERSE:
      return M_range<long>(src_size - start, src_size - start - (size - 1));
    }

    throw std::exception();
  }
  
}

#endif
