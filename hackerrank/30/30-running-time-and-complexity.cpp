#include <cmath>
#include <cstdio>
#include <vector>
#include <iostream>
#include <algorithm>
using namespace std;

bool isPrime(int num)
{
  if (num <= 1)
    return false;
  if (num <= 3)
    return true;

  int range = sqrt(num);
  if (num % 2 == 0 || num % 3 == 0)
    return false;

  for (int i = 5; i <= range; i += 6)
  {
    if (num % i == 0 || num % (i + 2) == 0)
    {
      return false;
    }
  }

  return true;
}

int main()
{
  int n;

  cin >> n;

  for (int i = 0; i < n; i++)
  {
    int num;
    cin >> num;

    cout << (isPrime(num) ? "Prime" : "Not prime") << endl;
  }

  return 0;
}
