#include <iostream>
#include <string>
#include <vector>

struct A
{
  int B;
  float C;
  std::string D;
  std::vector<bool> E;
};

struct A f(int p)
{
  A a;
  a.B = p;
  a.D = "asd";
  a.E.push_back(p > 0);
  return a;
}

int main()
{
  A a = f(1);
  std::cout << a.E[0] << std::endl;
  return 0;
}
