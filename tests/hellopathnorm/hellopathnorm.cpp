// SPDX-FileCopyrightText: 2026 seer contributors
//
// SPDX-License-Identifier: GPL-3.0-or-later

// Self-checking regression test for the path normalization layers.
//
//   1. Seer::normalizeSourcePath  — gdb command layer (separators only)
//   2. Seer::normalizeFileKey     — key layer (separators + case folding)
//   3. GdbMonitor                 — record-level path-field
//                                   normalization ('fullname='/'file='
//                                   values, MI octal escapes resolved),
//                                   fed real MI wire lines through a fake
//                                   'gdb' process
//
// Expectations are platform-conditional by design: Windows transforms,
// everything else is the identity. Exit code 0 = all checks passed.
//
// Build (Linux) : make
// Build (Win)   : see README.md

#include "SeerUtl.h"
#include "GdbMonitor.h"
#include <QtCore/QCoreApplication>
#include <QtCore/QProcess>
#include <QtCore/QFile>
#include <QtCore/QDir>
#include <QtCore/QTimer>
#include <QtCore/QDebug>
#include <iostream>

static int g_failures = 0;
static int g_checks   = 0;

static void check (const char* name, const QString& got, const QString& expected) {

    g_checks++;

    if (got == expected) {
        std::cout << "  ok   " << name << std::endl;
    }else{
        g_failures++;
        std::cout << "  FAIL " << name << std::endl
                  << "       got      [" << got.toStdString() << "]" << std::endl
                  << "       expected [" << expected.toStdString() << "]" << std::endl;
    }
}

int main (int argc, char* argv[]) {

    QCoreApplication app(argc, argv);

    std::cout << "Seer::normalizeSourcePath (gdb command layer)" << std::endl;

    check("backslash path", Seer::normalizeSourcePath("E:\\Github\\Tools\\x.cpp"),
#if defined(Q_OS_WIN)
        "E:/Github/Tools/x.cpp"
#else
        "E:\\Github\\Tools\\x.cpp"
#endif
        );

    check("forward path is identity", Seer::normalizeSourcePath("E:/Github/x.cpp"), "E:/Github/x.cpp");
    check("no separators is identity", Seer::normalizeSourcePath("plain.cpp"), "plain.cpp");
    check("empty is identity", Seer::normalizeSourcePath(""), "");

    std::cout << "Seer::normalizeFileKey (key layer)" << std::endl;

    check("backslash + case folding", Seer::normalizeFileKey("E:\\Github\\Tools\\X.cpp"),
#if defined(Q_OS_WIN)
        "e:/github/tools/x.cpp"
#else
        "E:\\Github\\Tools\\X.cpp"
#endif
        );

    check("forward + case folding", Seer::normalizeFileKey("e:/GITHUB/x.cpp"),
#if defined(Q_OS_WIN)
        "e:/github/x.cpp"
#else
        "e:/GITHUB/x.cpp"
#endif
        );

    // The same file under different spellings must land on one key.
    bool sameKey = (Seer::normalizeFileKey("E:\\Github\\X.cpp") == Seer::normalizeFileKey("e:/github/x.cpp"));
    check("same file, different spellings -> same key", sameKey ? "yes" : "no",
#if defined(Q_OS_WIN)
        "yes"
#else
        "no"     // identity on purpose: Linux filenames are case sensitive
#endif
        );

    std::cout << "GdbMonitor record-level fullname normalization" << std::endl;

    // Real MI wire forms. gdb 17.x escapes a backslash as '\\\\'.
    const QString starBackslash = "*stopped,frame={addr=\"0xe91569\",func=\"main\",file=\"E:/A/x.cpp\",fullname=\"E:\\\\A\\\\x.cpp\",line=\"6\"},thread-id=\"1\"";
    const QString doneTwoFields = "^done,stack=[frame={level=\"0\",func=\"main\",file=\"x.cpp\",fullname=\"E:/B/x.cpp\"},frame={level=\"1\",func=\"f\",file=\"y.cpp\",fullname=\"E:\\\\B\\\\y.cpp\"}]";
    const QString tildeEscaped  = "~\"program printed fullname=\\\"E:\\\\C\\\\x.cpp\\\" done\\n\"";
    const QString groupStarted  = "=thread-group-started,id=\"i1\",pid=\"1234\"";
    // MI octal escapes: gdb emits non-ASCII path bytes as \NNN octal
    // sequences on the wire. The bytes are the raw filename bytes from
    // the inferior's debug info — the ANSI code page for cmd/MSVC
    // builds and UTF-8 for MSYS2 builds. The first sample below is
    // GBK bytes (invalid UTF-8, exercises the code-page fallback); the
    // expected value is derived from the active code page, not
    // hard-coded, so the test stays portable across Windows locales.
    // The UTF-8 sample after it exercises the strict-UTF-8 path.
    const QString gbkBytes  = QString::fromLocal8Bit("\326\320\316\304\324\264\302\353");  // '中文源码' in GBK octal
    const QString octalEscape   = "*stopped,frame={addr=\"0xe91569\",func=\"main\",file=\"helloworld.cpp\",fullname=\"E:/P/"
        "\\326\\320\\316\\304\\324\\264\\302\\353"
        "/helloworld.cpp\",line=\"17\"}";
    // Expected: escapes resolved through the active code page.
    const QString octalEscapeExpected = "*stopped,frame={addr=\"0xe91569\",func=\"main\",file=\"helloworld.cpp\",fullname=\"E:/P/" + gbkBytes + "/helloworld.cpp\",line=\"17\"}";
    // MI escapes both path fields: 'file=' carries the same escaped
    // bytes as 'fullname=' and must be rewritten too. UTF-8 octals on
    // every platform (valid UTF-8 also decodes as UTF-8 on Windows).
    const QString octalFileField = "^done,files=[{file=\"E:/P/"
        "\\344\\270\\255\\346\\226\\207\\346\\272\\220\\347\\240\\201/hello\\344\\270\\255\\346\\226\\207.cpp"
        "\",fullname=\"E:/P/"
        "\\344\\270\\255\\346\\226\\207\\346\\272\\220\\347\\240\\201/hello\\344\\270\\255\\346\\226\\207.cpp"
        "\",debug-fully-read=\"true\"}]";
    const QString octalFileFieldExpected = "^done,files=[{file=\"E:/P/中文源码/hello中文.cpp\",fullname=\"E:/P/中文源码/hello中文.cpp\",debug-fully-read=\"true\"}]";
    // A literal backslash in the filename arrives wire-escaped as '\\'
    // ('\141' here is five literal characters: backslash, one, four,
    // one). The '\\141' pair must unescape atomically to '\141' text
    // — the trailing '141' must never be eaten as an octal escape.
    const QString literalBackslash = "*stopped,frame={addr=\"0xe91569\",func=\"main\",file=\"/tmp/\\\\141.c\",fullname=\"/tmp/\\\\141.c\",line=\"6\"}";
    // Linux keeps the wire form ('\\141') for downstream escape
    // handling; Windows treats the '\\' pair as an escaped separator
    // ('/' + the literal '141' text survives — proof the pair was
    // consumed atomically and '141' was not eaten as an octal escape).
    const QString literalBackslashExpected =
#if defined(Q_OS_WIN)
        "*stopped,frame={addr=\"0xe91569\",func=\"main\",file=\"/tmp//141.c\",fullname=\"/tmp//141.c\",line=\"6\"}";
#else
        literalBackslash;
#endif
#if defined(Q_OS_WIN)
    // Ambiguity sample: GBK '源' is bytes d4 b4, which are ALSO valid
    // UTF-8 (it decodes to an Armenian letter). The monitor must pick
    // the candidate that names an existing file: the test creates the
    // code-page spelling on disk, so the local-8-bit candidate wins.
    const QString ambiguousBytes = QString::fromLocal8Bit("\324\264");  // '源' in GBK
    QDir ambiguityDir(QDir::tempPath() + "/hellopathnorm-" + ambiguousBytes);
    ambiguityDir.removeRecursively();
    QDir().mkpath(ambiguityDir.absolutePath());
    // The disambiguation probes the filesystem for the exact reported
    // path, so the sample file itself must exist on disk.
    {
        QFile probe(ambiguityDir.absolutePath() + "/x.c");
        if (probe.open(QIODevice::WriteOnly) == false) {
            qCritical() << "Can't create" << probe.fileName();
            return 1;
        }
        probe.close();
    }
    const QString octalAmbiguous = "*stopped,frame={addr=\"0xe91569\",func=\"main\",file=\"x.c\",fullname=\""
        + QDir::tempPath() + "/hellopathnorm-\\324\\264/x.c\",line=\"6\"}";
    const QString octalAmbiguousExpected = "*stopped,frame={addr=\"0xe91569\",func=\"main\",file=\"x.c\",fullname=\""
        + QDir::tempPath() + "/hellopathnorm-" + ambiguousBytes + "/x.c\",line=\"6\"}";
#endif

    // Expected forms after the monitor.
    const QString starBackslashExpected =
#if defined(Q_OS_WIN)
        "*stopped,frame={addr=\"0xe91569\",func=\"main\",file=\"E:/A/x.cpp\",fullname=\"E:/A/x.cpp\",line=\"6\"},thread-id=\"1\"";
#else
        starBackslash;
#endif
    const QString doneTwoFieldsExpected =
#if defined(Q_OS_WIN)
        "^done,stack=[frame={level=\"0\",func=\"main\",file=\"x.cpp\",fullname=\"E:/B/x.cpp\"},frame={level=\"1\",func=\"f\",file=\"y.cpp\",fullname=\"E:/B/y.cpp\"}]";
#else
        doneTwoFields;
#endif

    // Write the sample stream to a temp file and 'cat' it back through a
    // fake gdb process, wired to a GdbMonitor exactly like SeerGdbWidget.
    const QString samplePath = QDir::tempPath() + "/hellopathnorm-mi.txt";
    {
        QFile f(samplePath);
        if (f.open(QIODevice::WriteOnly) == false) {
            qCritical() << "Can't open" << samplePath;
            return 1;
        }
        f.write((starBackslash + "\n").toUtf8());
        f.write((doneTwoFields + "\n").toUtf8());
        f.write((tildeEscaped  + "\n").toUtf8());
        f.write((groupStarted  + "\n").toUtf8());
        f.write((octalEscape   + "\n").toUtf8());
        f.write((octalFileField + "\n").toUtf8());
        f.write((literalBackslash + "\n").toUtf8());
#if defined(Q_OS_WIN)
        f.write((octalAmbiguous + "\n").toUtf8());
#endif
        f.close();
    }

    QProcess fake;
#if defined(Q_OS_WIN)
    fake.setProgram("cmd.exe");
    fake.setArguments(QStringList() << "/c" << "type" << QDir::toNativeSeparators(samplePath));
#else
    fake.setProgram("cat");
    fake.setArguments(QStringList() << samplePath);
#endif

    GdbMonitor monitor;
    QObject::connect(&fake, &QProcess::readyReadStandardOutput, &monitor, &GdbMonitor::handleReadyReadStandardOutput);

    QStringList collected;
    QObject::connect(&monitor, &GdbMonitor::tildeTextOutput,    [&collected](const QString& t) { collected << t; });
    QObject::connect(&monitor, &GdbMonitor::equalTextOutput,    [&collected](const QString& t) { collected << t; });
    QObject::connect(&monitor, &GdbMonitor::astrixTextOutput,   [&collected](const QString& t) { collected << t; });
    QObject::connect(&monitor, &GdbMonitor::caretTextOutput,    [&collected](const QString& t) { collected << t; });
    QObject::connect(&monitor, &GdbMonitor::ampersandTextOutput,[&collected](const QString& t) { collected << t; });
    QObject::connect(&monitor, &GdbMonitor::atsignTextOutput,   [&collected](const QString& t) { collected << t; });

    fake.start();
    fake.waitForFinished(10000);
    fake.waitForReadyRead(1000);
    QCoreApplication::processEvents();

    const int expectedLines =
#if defined(Q_OS_WIN)
        8;
#else
        7;
#endif
    check("collected line count", QString::number(collected.size()), QString::number(expectedLines));

    if (collected.size() >= 6) {
        check("*stopped fullname converted",      collected[0], starBackslashExpected);
        check("^done two fullnames converted",    collected[1], doneTwoFieldsExpected);
        check("~ escaped text untouched",         collected[2], tildeEscaped);
        check("= record without fullname intact", collected[3], groupStarted);
        check("octal escapes -> codepage path",   collected[4], octalEscapeExpected);
        check("octal escapes -> file field too", collected[5], octalFileFieldExpected);
    }
    if (collected.size() >= 7) {
        check("wire '\\\\' unescapes atomically", collected[6], literalBackslashExpected);
    }
#if defined(Q_OS_WIN)
    if (collected.size() >= 8) {
        check("ambiguous bytes -> existing file", collected[7], octalAmbiguousExpected);
        QDir(QDir::tempPath() + "/hellopathnorm-" + ambiguousBytes).removeRecursively();
    }
#endif

    std::cout << "================ " << (g_checks - g_failures) << "/" << g_checks << " checks passed" << std::endl;

    return g_failures == 0 ? 0 : 1;
}
