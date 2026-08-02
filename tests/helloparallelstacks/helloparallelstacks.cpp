/*

stack 1: main() -> readFiles()
stack 2: threadReader() -> readFile() -> openFile()
stack 3: threadReader() -> readFile() -> closeFile()
stack 4: threadReader() -> readFile() -> readLine() -> read()
stack 5: threadReader() -> readFile() -> readLine() -> seek()

*/


#include <print>
#include <vector>
#include <string>
#include <thread>
#include <chrono>

void openFile(const std::string& filename)
{
    if (filename == "file1.dat") {
        std::this_thread::sleep_for(std::chrono::seconds{60});
    }
}

void closeFile(const std::string& filename)
{
    if (filename == "file2.dat") {
        std::this_thread::sleep_for(std::chrono::seconds{60});
    }
}

void read(const std::string& filename)
{
    if (filename == "file3.dat") {
        std::this_thread::sleep_for(std::chrono::seconds{60});
    }
}

void seek(const std::string& filename)
{
    if (filename == "file4.dat") {
        std::this_thread::sleep_for(std::chrono::seconds{60});
    }
}

void readLine(const std::string& filename)
{
    seek(filename);
    read(filename);
}

void readFile(const std::string& filename)
{
    openFile(filename);
    readLine(filename);
    closeFile(filename);
}

void threadReader(const std::string& filename)
{
    readFile(filename);
}

void readFiles(const std::vector<std::string> & filenames)
{
    std::vector<std::thread> readers;

    for (const auto& filename: filenames) {
        readers.emplace_back(threadReader, filename);
    }

    for (auto& reader: readers) {
        reader.join();
    }
}

int main()
{
    auto filenames = std::vector<std::string>{"file1.dat", "file2.dat", "file3.dat", "file4.dat"};
    readFiles(filenames);
}