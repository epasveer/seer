#include <iostream>
#include <vector>

using namespace std;

int main(void) {

    size_t idx = 0;

    vector<int> v{1, 2, 3, 4, 5};

    auto& vr = v;

    for (auto i : v) {
        idx++;
        cout << i << "=" << v[i] << endl;
    }

    return 0;
}

