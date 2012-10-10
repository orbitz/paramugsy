#include <iostream>

#include <m_range.hh>
#include <m_profile.hh>

using namespace std;
using namespace Para_mugsy;

int main(int argc, char **argv) {
  Profile profile = read_profile_file(true, argv[1]);

  cout << profile.p_major_name << " " << profile.p_minor_name << endl;
  cout << "Sequence = (" << profile.p_range.get_start() << ", " << profile.p_range.get_end() << ")" << endl;
  cout << "Length = " << profile.p_length << endl;
  for(std::vector<M_range<M_profile_idx> >::const_iterator i = profile.p_gaps.begin();
      i != profile.p_gaps.end();
      ++i) {
    cout << "Gap: (" << i->get_start() << ", " << i->get_end() << ")" << endl;
  }

  cout << "profile_idx_of_seq_idx(profile, 3332002) = 1 = " << profile_idx_of_seq_idx(profile, 3332002) << endl;
  cout << "seq_idx_of_profile_idx(profile, 1) = 3332002 = " << seq_idx_of_profile_idx(profile, 1).value() << endl;

  cout << "profile_idx_of_seq_idx(profile, 3411545) = 79551 = " << profile_idx_of_seq_idx(profile, 3411545) << endl;
  cout << "seq_idx_of_profile_idx(profile, 79551) = 3411545 = " << seq_idx_of_profile_idx(profile, 79551).value() << endl;

  cout << "seq_idx_of_profile_idx(profile, 681).is_none = true = " << seq_idx_of_profile_idx(profile, 681).is_none() << endl;

  return 0;
}
