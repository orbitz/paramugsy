#include <iostream>
#include <m_option.hh>

using namespace std;
using namespace Para_mugsy;

int main() {
  M_option<int> has_value(5);
  M_option<int> has_no_value;

  cout << "has_value: " << has_value.is_none() << endl;
  cout << "has_no_value: " << has_no_value.is_none() << endl;
  cout << "has_value.value: " << has_value.value() << endl;
  cout << "has_no_value.value: " << has_no_value.value() << endl;
  
  return 0;
}
