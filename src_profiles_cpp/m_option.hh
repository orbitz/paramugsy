#include <exception>

namespace Para_mugsy {
  class Is_none_error : public std::exception {};
  
  template <typename T>
  class M_option {
  public:
    M_option() : val(), is_none_(true) {}
    M_option(T const& t) : val(t), is_none_(false) {}

    T const& value() {
      if(!is_none()) {
        return val;
      }
      else {
        throw Is_none_error();
      }
    }

    bool is_none() {
      return is_none_;
    }
    
  private:
    T val;
    bool is_none_;
  };
}
      
