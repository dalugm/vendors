#include <cstdlib>
#include <cmath>
#include <ctime>
#include <iostream>

using namespace std;

// return a uniformly distributed random number
double RandomGenerator()
{
  return ((double)(rand()) + 1.) / ((double)(RAND_MAX) + 1.);
}

// return a normally distributed random number
double NormalRandom()
{
  double y1 = RandomGenerator();
  double y2 = RandomGenerator();

  return cos(2 * 3.14 * y2) * sqrt(-2. * log(y1));
}

int main()
{
  int number;
  double variance, medium;

  while (1) {
    cout << endl << "Input medium value, variance and the number of random numbers: " << endl;
    cin >> medium >> variance >> number;

    for (int i = 0; i < number; i++) {
      double x = NormalRandom() * variance + medium;
      cout << x << endl;
    }
  }

  return 0;
}
