#include <iostream>
#include <cstring>
using namespace std;

int main()
{
  long long b[5001];
  long long s[5001];
  long long ans = 0;
  
  int k, n;
  int i, j;
  
  cin >> k >> n;
  for (i = 1; i <= n; ++i)
  {
    b[i] = 1;
    cin >> s[i];
  }
  
  for (i = 2; i <= k; ++i)
    for (j = 2; j <= n; ++j)
      b[j] = (b[j] + b[j - 1]) % 998244353;
  
  for (i = 1; i <= n; ++i)
  {
    ans += s[i] * b[n - i + 1] % 998244353;
  }
  cout << ans;
  return 0;
}