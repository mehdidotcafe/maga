#include <cmath>
#include <cstdio>
#include <vector>
#include <iostream>
#include <algorithm>

using namespace std;

class Difference {
    private:
    vector<int> elements;
  
  	public:
  	int maximumDifference;

    Difference(vector<int> &_elements) {
      this->elements = _elements;
    }

    void computeDifference() {
      int elementsSize = this->elements.size();

      for (int i = 0; i < elementsSize; ++i) {
        for (int j = i + i; j < elementsSize; ++j) {
          this->maximumDifference = fmax(this->maximumDifference, abs(this->elements[i] - this->elements[j]));
        }
      }
    }

};

int main() {
    int N;
    cin >> N;
    
    vector<int> a;
    
    for (int i = 0; i < N; i++) {
        int e;
        cin >> e;
        
        a.push_back(e);
    }
    
    Difference d(a);
    
    d.computeDifference();
    
    cout << d.maximumDifference;
    
    return 0;
}