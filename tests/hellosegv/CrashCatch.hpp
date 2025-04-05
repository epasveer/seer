#pragma once

/*
CrashCatch - A simple cross-platform crash handler
Version 1.2.0
Created by Keith Pottratz
Email: keithpotz@gmail.com
License: MIT
*/

#include <string>
#include <sstream>
#include <fstream>
#include <iostream>
#include <iomanip>
#include <chrono>
#include <filesystem>
#include <functional>

#if defined(_WIN32)
#define CRASHCATCH_PLATFORM_WINDOWS
#include <Windows.h>
#include <DbgHelp.h>
#pragma comment(lib, "DbgHelp.lib") //Auto-link debugging support library
#elif defined(__linux__)
#define CRASHCATCH_PLATFORM_LINUX
#include <signal.h>
#include <execinfo.h>
#include <unistd.h>
#include <limits.h>
#include <cxxabi.h>
#include <string.h>
#endif

namespace CrashCatch {
    // Config structure to customize CrashCatch behaviour
    struct Config {
		std::string dumpFolder = "./crash_dumps/"; 	// Default folder for crash dumps
		std::string dumpFileName = "crash"; 		// Default file name for crash dumps
		bool enableTextLog = true; 			// Enable text log for crash dumps
		bool autoTimestamp = true; 			// Automatically append timestamp to dump file name
        bool showCrashDialog = false; 				// Windows only
		std::function<void()> onCrash = nullptr; 	// Callback function to execute on crash
		std::string appVersion = "unknown";	 	// Application version
        std::string buildConfig = 
#ifdef _DEBUG
            "Debug";
#else
            "Release";
#endif
		std::string additionalNotes = "";  // Additional notes to include in the crash log
    };

	inline Config globalConfig; // Global configuration object used internally
    // Utility: Generate a timestamp string
    inline std::string getTimestamp() {
        auto now = std::chrono::system_clock::now();
        auto time = std::chrono::system_clock::to_time_t(now);
        std::stringstream ss;
        ss << std::put_time(std::localtime(&time), "%Y-%m-%d_%H-%M-%S");
        return ss.str();
    }

    inline std::string getExecutablePath() {
#ifdef CRASHCATCH_PLATFORM_WINDOWS
        char buffer[MAX_PATH];
        GetModuleFileNameA(nullptr, buffer, MAX_PATH);
        return std::string(buffer);
#elif defined(CRASHCATCH_PLATFORM_LINUX)
        char path[PATH_MAX];
        ssize_t len = readlink("/proc/self/exe", path, sizeof(path) - 1);
        if (len != -1) {
            path[len] = '\0';
            return std::string(path);
        }
        return "(unknown)";
#endif
    }

#ifdef CRASHCATCH_PLATFORM_LINUX
    inline std::string demangle(const char* symbol) {
        size_t size;
        int status;
        char* demangled = abi::__cxa_demangle(symbol, nullptr, &size, &status);
        std::string result = (status == 0) ? demangled : symbol;
        free(demangled);
        return result;
    }
#endif
	// Collect Diagnositcs for crash log (version, build config, platform, executable path)
    inline std::string getDiagnosticsInfo() {
        std::stringstream ss;
        ss << "App Version: " << globalConfig.appVersion << "\n";
        ss << "Build Config: " << globalConfig.buildConfig << "\n";
#ifdef CRASHCATCH_PLATFORM_WINDOWS
        ss << "Platform: Windows\n";
#elif defined(CRASHCATCH_PLATFORM_LINUX)
        ss << "Platform: Linux\n";
#endif
        ss << "Executable: " << getExecutablePath() << "\n";
        if (!globalConfig.additionalNotes.empty()) {
            ss << "Notes: " << globalConfig.additionalNotes << "\n";
        }
        return ss.str();
    }
    // Write Crash summary .txt file
    inline void writeCrashLog(const std::string& logPath, const std::string& timestamp, int signal = 0) {
        std::ofstream log(logPath);
        if (!log.is_open()) return;

        log << "Crash Report\n============\n";

#ifdef CRASHCATCH_PLATFORM_LINUX
        // Stack trace for Linux
        log << "Signal: " << strsignal(signal) << " (" << signal << ")\n";
#endif
        log << "Timestamp: " << (timestamp.empty() ? "N/A" : timestamp) << "\n\n";
        log << "Environment Info:\n" << getDiagnosticsInfo() << "\n";

#ifdef CRASHCATCH_PLATFORM_LINUX
        void* callstack[128];
        int frames = backtrace(callstack, 128);
        char** symbols = backtrace_symbols(callstack, frames);
        log << "\nStack Trace:\n";
        for (int i = 0; i < frames; ++i) {
            std::string raw = symbols[i];
            std::string demangled = demangle(raw.c_str());
            log << "  [" << i << "]: " << demangled << "\n";
        }
        free(symbols);
#endif

        log.close();
    }

#ifdef CRASHCATCH_PLATFORM_WINDOWS
	// Windows-specific crash handler
    inline LONG WINAPI UnhandledExceptionHandler(EXCEPTION_POINTERS* ep) {
        if (globalConfig.onCrash) globalConfig.onCrash();
        std::filesystem::create_directories(globalConfig.dumpFolder);
        std::string timestamp = globalConfig.autoTimestamp ? getTimestamp() : "";
        std::string base = globalConfig.dumpFileName + (timestamp.empty() ? "" : ("_" + timestamp));
        std::string dumpPath = globalConfig.dumpFolder + base + ".dmp";

        HANDLE hFile = CreateFileA(dumpPath.c_str(), GENERIC_WRITE, 0, nullptr, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, nullptr);
        if (hFile != INVALID_HANDLE_VALUE) {
            MINIDUMP_EXCEPTION_INFORMATION dumpInfo = { GetCurrentThreadId(), ep, FALSE };
            MiniDumpWriteDump(GetCurrentProcess(), GetCurrentProcessId(), hFile, MiniDumpWithDataSegs, &dumpInfo, nullptr, nullptr);
            CloseHandle(hFile);

            if (globalConfig.enableTextLog) {
                std::string logPath = globalConfig.dumpFolder + base + ".txt";
                writeCrashLog(logPath, timestamp);
            }

            if (globalConfig.showCrashDialog) {
                std::string msg = "Crash occurred. Dump written to:\n" + dumpPath;
                MessageBoxA(nullptr, msg.c_str(), "Crash Detected", MB_OK | MB_ICONERROR);
            }
        }
        return EXCEPTION_EXECUTE_HANDLER;
    }
#endif

#ifdef CRASHCATCH_PLATFORM_LINUX
	//Linux signal handler: creates .txt crash log with stack trace
    inline void linuxSignalHandler(int signum) {
        if (globalConfig.onCrash) globalConfig.onCrash();
        std::filesystem::create_directories(globalConfig.dumpFolder);
        std::string timestamp = globalConfig.autoTimestamp ? getTimestamp() : "";
        std::string base = globalConfig.dumpFileName + (timestamp.empty() ? "" : ("_" + timestamp));
        std::string logPath = globalConfig.dumpFolder + base + ".txt";
        writeCrashLog(logPath, timestamp, signum);
		_exit(1); // Exit immediately after handling signal
    }
#endif
	//Main initialization function: sets up the crash handler
    inline bool initialize(const Config& config = Config()) {
        globalConfig = config;
#ifdef CRASHCATCH_PLATFORM_WINDOWS
        SetUnhandledExceptionFilter(UnhandledExceptionHandler);
#elif defined(CRASHCATCH_PLATFORM_LINUX)
		signal(SIGSEGV, linuxSignalHandler); // Segmentation fault
		signal(SIGABRT, linuxSignalHandler); // Abort signal
		signal(SIGFPE, linuxSignalHandler); // Floating point exception
		signal(SIGILL, linuxSignalHandler); // Illegal instruction
		signal(SIGBUS, linuxSignalHandler); // Misaligned memory address
#endif
        return true;
    }
	//One liner to enable crash handling with default config
    inline bool enable() { return initialize(Config{}); }
    
    //Optional Auto enable crash handler
#ifdef CRASHCATCH_AUTO_INIT
    namespace {
        const bool _autoInit = CrashCatch::enable();
    }
#endif

} // namespace CrashCatch
