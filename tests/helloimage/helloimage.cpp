#include <string>
#include <iostream>
#include <fstream>
#include <filesystem>

int main (int argc, char** argv) {

    std::cout << "Hello, Image!" << std::endl;

    // Allocate and read BMP
    std::uintmax_t filesize_bmp = std::filesystem::file_size("lena.bmp");

    char* buffer_bmp = (char*)malloc(filesize_bmp);

    std::ifstream fin_bmp("lena.bmp", std::ios::in | std::ios::binary );

    if (!fin_bmp) {
        std::cout << "Can't open BMP file." << std::endl;
        return 1;
    }

    fin_bmp.read(buffer_bmp, filesize_bmp);

    int n_bmp = fin_bmp.gcount();

    // Allocate and read PNG
    std::uintmax_t filesize_png = std::filesystem::file_size("lena.png");

    char* buffer_png = (char*)malloc(filesize_png);

    std::ifstream fin_png("lena.png", std::ios::in | std::ios::binary );

    if (!fin_png) {
        std::cout << "Can't open PNG file." << std::endl;
        return 1;
    }

    fin_png.read(buffer_png, filesize_png);

    int n_png = fin_png.gcount();

    // Free things
    free(buffer_bmp);
    free(buffer_png);

    return 0;
}

