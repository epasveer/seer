#include <cstdio>
#include <memory>
#include <vector>

using namespace std;

void foo(int i, int j) {
    printf("%d %d\n", i, j);
}

int main() {

    auto i = make_unique<int>(3);

    vector<int> v{1,2};

    foo(*i, v.back()); // step into
}

