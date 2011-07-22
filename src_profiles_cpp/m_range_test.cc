#include <iostream>

#include <m_range.hh>

using namespace std;
using namespace Para_mugsy;

int main() {
  M_range<int> r_1(1, 3);

  cout << "r_1.length: " << r_1.length() << endl;

  return 0;
}
