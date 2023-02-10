#include <bits/stdc++.h>

using namespace std;

int main()
{
    string S;
    getline(cin, S);

  try {
    cout << stoi(S) << endl;
  } catch (...) {
    cout << "Bad String" << endl;
  }

    return 0;
}
