// helloprimethreads.cpp
// Spawns a handful of named threads; each one hunts for primes for about a minute.
//
// Linux only.
// Build: g++ -std=c++17 -O2 -pthread helloprimethreads.cpp -o helloprimethreads
// Run:   ./helloprimethreads        (default 60 seconds)
//        ./helloprimethreads 10     (run for 10 seconds instead)

#include <chrono>
#include <cstdint>
#include <cstdlib>
#include <iostream>
#include <mutex>
#include <string>
#include <thread>
#include <vector>

#include <pthread.h>

// Give the current thread an OS-level name so it shows up in debuggers,
// top -H, htop, ps -L, gdb's "info threads", etc.
// Linux limits thread names to 15 characters (plus the terminating null).
void setCurrentThreadName(const std::string& name) {
    pthread_setname_np(pthread_self(), name.substr(0, 15).c_str());
}

std::mutex coutMutex;  // keeps lines from different threads from interleaving

void log(const std::string& msg) {
    std::lock_guard<std::mutex> lock(coutMutex);
    std::cout << msg << std::endl;
}

bool isPrime(std::uint64_t n) {
    if (n < 2) return false;
    if (n % 2 == 0) return n == 2;
    for (std::uint64_t d = 3; d * d <= n; d += 2)
        if (n % d == 0) return false;
    return true;
}

// The work each thread does: test numbers for primality until time runs out.
// Each thread starts at a different point so they aren't all doing identical work.
void primeWorker(const std::string& name, std::uint64_t start, std::chrono::seconds duration) {
    setCurrentThreadName(name);
    log("[" + name + "] starting at " + std::to_string(start));

    auto deadline = std::chrono::steady_clock::now() + duration;
    auto nextReport = std::chrono::steady_clock::now() + std::chrono::seconds(10);

    std::uint64_t n = start, count = 0, largest = 0;
    while (std::chrono::steady_clock::now() < deadline) {
        // Check the clock only every 1000 numbers to keep overhead low.
        for (int i = 0; i < 1000; ++i, ++n) {
            if (isPrime(n)) { ++count; largest = n; }
        }
        if (std::chrono::steady_clock::now() >= nextReport) {
            log("[" + name + "] progress: " + std::to_string(count) + " primes so far");
            nextReport += std::chrono::seconds(10);
        }
    }

    log("[" + name + "] done: found " + std::to_string(count) +
        " primes, largest = " + std::to_string(largest));
}

int main(int argc, char* argv[]) {
    int seconds = (argc > 1) ? std::atoi(argv[1]) : 60;
    if (seconds <= 0) seconds = 60;

    const std::vector<std::string> names = { "Alpha", "Bravo", "Charlie", "Delta", "Echo" };

    std::cout << "Launching " << names.size() << " threads for "
              << seconds << " seconds each...\n";

    std::vector<std::thread> threads;
    for (std::size_t i = 0; i < names.size(); ++i) {
        std::uint64_t start = 1'000'000'000ULL * (i + 1);   // 1e9, 2e9, 3e9, ...
        threads.emplace_back(primeWorker, names[i], start, std::chrono::seconds(seconds));
    }

    for (auto& t : threads) t.join();   // wait for all workers to finish

    std::cout << "All threads finished.\n";
    return 0;
}

