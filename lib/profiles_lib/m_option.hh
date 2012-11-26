/*
 * This is an inadequate attempt at an option type. Things that need fixing:
 * - Can we male `val` not be a pointer somehow?  Would be nice if we could
 *   keep the lifetime of val to that of the M_option without allocating in freestore
 * - Should be a shared pointer so we don't have to copy so much
 */
#ifndef M_OPTION_HH
#define M_OPTION_HH

#include <exception>


namespace Para_mugsy {
  class Is_none_error : public std::exception {};

  template <typename T>
  class M_option {
  public:
    M_option() : val(0) {}
    M_option(T const& t) : val(new T(t)) {}
    M_option(M_option<T> const& option) : val(option.val ? new T(*option.val) : 0) {}

    T const& value() const {
      if(!is_none()) {
        return *val;
      }
      else {
        throw Is_none_error();
      }
    }

    bool is_none() const {
      return 0 == val;
    }

    operator bool() const {
      return !!val;
    }

    ~M_option() {
      delete val;
    }

  private:
    T const * const val;
  };
}

#endif
