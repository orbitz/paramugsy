#include <m_option.hh>
#include <m_direction.hh>

namespace Para_mugsy {
  
  template <typename T>
  class M_range {
  public:
    M_range(T const& s, T const& e) : start(s), end(e) {}

    T get_start() { return start; }
    T get_end() { return end; }
    
    unsigned long length() { return start <= end ? end - start : start - end; }
    
    M_direction get_direction() { return start <= end ? D_FORWARD : D_REVERSE; }

    M_range<T> reverse() { return M_range(end, start); }
    
  private:
    T start;
    T end;
    
  };

  template <typename T>
  M_range<T> make_forward(M_range<T> const& r) {
    switch(r.get_direction()) {
    D_FORWARD:
      return r;
    D_REVERSE:
      return r.reverse();
    }
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
  bool contains(M_range<T> const& range, T const& v) {
    M_range<T> r = make_forward(range);
    return r.get_start() <= v && v <= r.get_end();
  }

  template <typename T>
  M_range<T> of_pair(std::pair<T, T> const& p) {
    return M_range<T>(p.first, p.second);
  }

  template <typename T>
  std::pair<T, T> to_pair(M_range<T> const& r) {
    return std::make_pair(r.get_start(), r.get_end());
  }

}
