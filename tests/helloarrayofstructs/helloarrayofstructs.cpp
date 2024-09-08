#include <stdlib.h>
#include <iostream>

struct CoordsData {
    int x;
    int y;
    int z;
};

struct CoordsArray {
    struct CoordsData* data;
    int num;
};

int main(void) {

    struct CoordsArray arr;
    arr.num = 10;
    arr.data = (CoordsData*)malloc(sizeof(struct CoordsData) * arr.num);

    for (int i = 0; i < arr.num; ++i) {
        arr.data[i].x = i;
        arr.data[i].y = i + i;
        arr.data[i].z = i * i;

        std::cout << i << ") x=" << arr.data[i].x << " y=" << arr.data[i].y << " z=" << arr.data[i].z << std::endl;
    }

    return 0;
}

