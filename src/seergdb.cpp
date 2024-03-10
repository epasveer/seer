#include "SeerMainWindow.h"
#include "SeerUtl.h"
#include "QProcessInfo.h"
#include <QtWidgets/QApplication>
#include <QtGui/QIcon>
#include <QtCore/QCommandLineParser>
#include <QtCore/QCommandLineOption>
#include <QtCore/QStringList>
#include <QtCore/QString>
#include <QtCore/QTextStream>
#include <QtCore/QObject>
#include <QtCore/QDebug>
#include <iostream>

static void seerhelp() {

    QString text;
    QString fileName(":/seer/resources/help/seergdb.hlp");

    QFile file(fileName);

    if (file.open(QIODevice::ReadOnly) == false) {
        text = "seer: no help available.\n";
    }else{
        text = file.readAll();
    }

    file.close();

    QTextStream(stdout) << text;

    exit(0);
}

int main (int argc, char* argv[]) {

    //
    // Set up logging and message formats.
    //
    QLoggingCategory::setFilterRules("*.debug=false\n"
                                     "*.info=false\n"
                                     "*.warning=false\n"
                                     "*.critical=true");

    qSetMessagePattern("[%{time hh:mm:ss}][%{function}:%{line}][%{category}] %{message}");

    //
    // Create the app.
    //
    QApplication app(argc, argv);

    QCoreApplication::setApplicationName("seergdb");
    QCoreApplication::setOrganizationName("seergdb");
    QCoreApplication::setApplicationVersion(Seer::version() + " - Ernie Pasveer (c)2021 - 2024");

    //
    // Parse arguments.
    //
    QCommandLineParser parser;
    parser.setApplicationDescription("\nSeer - A gui frontend for gdb.");
    parser.setOptionsAfterPositionalArgumentsMode(QCommandLineParser::ParseAsPositionalArguments); // Treat all arguments after the executable name as positional arguments too.
    parser.setSingleDashWordOptionMode(QCommandLineParser::ParseAsLongOptions); // Treat all arguments long options.
    parser.addVersionOption();

    // Run or start options.
    QCommandLineOption runOption(QStringList() << "r" << "run");
    parser.addOption(runOption);

    QCommandLineOption startOption(QStringList() << "s" << "start");
    parser.addOption(startOption);

    QCommandLineOption attachOption(QStringList() << "attach", "", "pid");
    parser.addOption(attachOption);

    QCommandLineOption connectOption(QStringList() << "connect", "", "medium");
    parser.addOption(connectOption);

    QCommandLineOption rrOption(QStringList() << "rr", "", "hostport");
    parser.addOption(rrOption);

    QCommandLineOption corefileOption(QStringList() << "core", "", "corefile");
    parser.addOption(corefileOption);

    QCommandLineOption projectOption(QStringList() << "project", "", "project");
    parser.addOption(projectOption);

    QCommandLineOption configOption(QStringList() << "config");
    parser.addOption(configOption);

    QCommandLineOption workingdirOption(QStringList() << "cwd" << "working-dir", "", "workingdirectory");
    parser.addOption(workingdirOption);

    QCommandLineOption symbolfileOption(QStringList() << "sym" << "symbol-file", "", "symbolfile");
    parser.addOption(symbolfileOption);

    QCommandLineOption breakfileOption(QStringList() << "bl" << "break-load", "", "breakpointfile");
    parser.addOption(breakfileOption);

    QCommandLineOption breakfunctionOption(QStringList() << "bf" << "break-function", "", "breakpointfunction");
    parser.addOption(breakfunctionOption);

    QCommandLineOption breaksourceOption(QStringList() << "bs" << "break-source", "", "breakpointsource");
    parser.addOption(breaksourceOption);

    QCommandLineOption showAssemblyTabOption(QStringList() << "sat" << "show-assembly-tab");
    parser.addOption(showAssemblyTabOption);

    QCommandLineOption startAddressRandomizeOption(QStringList() << "sar" << "start-address-randomize");
    parser.addOption(startAddressRandomizeOption);

    QCommandLineOption nonStopModeOption(QStringList() << "nsm" << "non-stop-mode");
    parser.addOption(nonStopModeOption);

    QCommandLineOption gdbProgramOption(QStringList() << "gdb-program", "", "gdbprogram");
    parser.addOption(gdbProgramOption);

    QCommandLineOption gdbArgumentsOption(QStringList() << "gdb-arguments", "", "gdbarguments");
    parser.addOption(gdbArgumentsOption);

    QCommandLineOption xxdebugOption(QStringList() << "xxx");
    parser.addOption(xxdebugOption);

    QCommandLineOption helpOption(QStringList() << "h" << "help");
    parser.addOption(helpOption);

    // A positional argument for executable name.
    // All other arguments after that are treated as positional arguments for the executable.
    parser.addPositionalArgument("executableandarguments", "");

    // Process the arguments.
    parser.process(app);

    if (parser.isSet(helpOption)) {
        seerhelp();
    }

    if (parser.isSet(xxdebugOption)) {
        QLoggingCategory::setFilterRules("*.debug=false\n"
                                         "*.info=false\n"
                                         "*.warning=false\n"
                                         "*.critical=true\n"
                                         "default.debug=true");
    }

    // Get the positional arguments. (The ones at the end of the line - executable name and its arguments.
    QStringList positionalArguments = parser.positionalArguments();

    // Get the executable name from the arguments;
    QString executableName;

    if (positionalArguments.size() > 0) {
        executableName = positionalArguments.takeFirst();
    }

    // Get launch mode.
    QString launchMode    = "none";
    QString breakMode     = "none";
    int     executablePid = -1;
    QString executableConnectHostPort;
    QString executableRRTraceDirectory;
    QString executableWorkingDirectory;
    QString executableSymbolFilename;
    QString executableBreakpointsFilename;
    QString executableBreakpointFunctionName;
    QString executableBreakpointSourceFilenameAndLineno;
    QString executableShowAssemblyTab;
    QString executableStartAddressRandomize;
    QString executableNonStopMode;
    QString executableCoreFilename;
    QString projectFilename;
    QString gdbProgram;
    QString gdbArguments;

    if (parser.isSet(runOption)) {
        launchMode = "run";
        breakMode  = "none";
    }

    if (parser.isSet(startOption)) {
        launchMode = "run";
        breakMode  = "inmain";
    }

    if (parser.isSet(workingdirOption)) {
        executableWorkingDirectory = parser.value(workingdirOption);
    }

    if (parser.isSet(symbolfileOption)) {
        executableSymbolFilename = parser.value(symbolfileOption);
    }

    if (parser.isSet(breakfileOption)) {
        executableBreakpointsFilename = parser.value(breakfileOption);
    }

    if (parser.isSet(breakfunctionOption)) {
        executableBreakpointFunctionName = parser.value(breakfunctionOption);
        breakMode  = "infunction";
    }

    if (parser.isSet(breaksourceOption)) {
        executableBreakpointSourceFilenameAndLineno = parser.value(breaksourceOption);
        breakMode  = "insource";
    }

    if (parser.isSet(showAssemblyTabOption)) {
        executableShowAssemblyTab = parser.value(showAssemblyTabOption);
    }

    if (parser.isSet(startAddressRandomizeOption)) {
        executableStartAddressRandomize = parser.value(startAddressRandomizeOption);
    }

    if (parser.isSet(nonStopModeOption)) {
        executableNonStopMode = parser.value(nonStopModeOption);
    }

    if (parser.isSet(attachOption)) {
        launchMode = "attach";

        QString pid = parser.value(attachOption);

        executablePid = pid.toInt();

        if (executableName == "") {

            QProcessInfo info = QProcessInfo::populate(executablePid);

            executableName = info.path() + "/" + info.name();
        }
    }

    if (parser.isSet(connectOption)) {
        launchMode = "connect";

        executableConnectHostPort = parser.value(connectOption);
    }

    if (parser.isSet(rrOption)) {
        launchMode = "rr";

        executableRRTraceDirectory = parser.value(rrOption);
    }

    if (parser.isSet(corefileOption)) {
        launchMode = "corefile";

        executableCoreFilename = parser.value(corefileOption);
    }

    if (parser.isSet(projectOption)) {
        launchMode = "project";

        projectFilename = parser.value(projectOption);
    }

    if (parser.isSet(configOption)) {
        launchMode = "configdialog";
    }

    if (parser.isSet(gdbProgramOption)) {
        gdbProgram = parser.value(gdbProgramOption);
    }

    if (parser.isSet(gdbArgumentsOption)) {
        gdbArguments = parser.value(gdbArgumentsOption);
    }


    //
    // Start Seer
    //
    SeerMainWindow seer;

    seer.setWindowIcon(QIcon(":/seer/resources/seergdb_64x64.png"));
    seer.setExecutableName(executableName);
    seer.setExecutableWorkingDirectory(executableWorkingDirectory);
    seer.setExecutableSymbolName(executableSymbolFilename);
    seer.setExecutableArguments(positionalArguments);

    if (executableBreakpointsFilename != "") {
        seer.setExecutableBreakpointsFilename(executableBreakpointsFilename);
    }

    if (executableBreakpointFunctionName != "") {
        seer.setExecutableBreakpointFunctionName(executableBreakpointFunctionName);
    }

    if (executableBreakpointSourceFilenameAndLineno != "") {
        seer.setExecutableBreakpointSourceName(executableBreakpointSourceFilenameAndLineno);
    }

    if (executableShowAssemblyTab != "") {
        if (executableShowAssemblyTab == "yes") {
            seer.setExecutableShowAssemblyTab(true);

        }else if (executableShowAssemblyTab == "no") {
            seer.setExecutableShowAssemblyTab(false);

        }else{
            printf("%s: Unknown --show-assembly-tab option '%s'\n", qPrintable(QCoreApplication::applicationName()), qPrintable(executableShowAssemblyTab));
            return 1;
        }
    }

    if (executableStartAddressRandomize != "") {
        if (executableStartAddressRandomize == "yes") {
            seer.setExecutableRandomizeStartAddress(true);

        }else if (executableStartAddressRandomize == "no") {
            seer.setExecutableRandomizeStartAddress(false);

        }else{
            printf("%s: Unknown --start-address-randomize option '%s'\n", qPrintable(QCoreApplication::applicationName()), qPrintable(executableStartAddressRandomize));
            return 1;
        }
    }

    if (executableNonStopMode != "") {
        if (executableNonStopMode == "yes") {
            seer.setExecutableNonStopMode(true);

        }else if (executableNonStopMode == "no") {
            seer.setExecutableNonStopMode(false);

        }else{
            printf("%s: Unknown --non-stop-mode option '%s'\n", qPrintable(QCoreApplication::applicationName()), qPrintable(executableNonStopMode));
            return 1;
        }
    }

    if (gdbProgram != "") {
        seer.setGdbProgramOverride(gdbProgram);
    }

    if (gdbArguments != "") {
        seer.setGdbArgumentsOverride(gdbArguments);
    }

    qDebug() << "EXECUTABLENAME"    << executableName;
    qDebug() << "WORKINGDIRECTORY"  << executableWorkingDirectory;
    qDebug() << "SYMBOLNAME"        << executableSymbolFilename;
    qDebug() << "PID"               << executablePid;
    qDebug() << "CONNECTHOST"       << executableConnectHostPort;
    qDebug() << "RRTRACEDIRECTORY"  << executableRRTraceDirectory;
    qDebug() << "COREFILENAME"      << executableCoreFilename;
    qDebug() << "PROJECTFILE"       << projectFilename;
    qDebug() << "ARGUMENTS"         << positionalArguments;

    seer.setExecutablePid(executablePid);
    seer.setExecutableConnectHostPort(executableConnectHostPort);
    seer.setExecutableRRTraceDirectory(executableRRTraceDirectory);
    seer.setExecutableCoreFilename(executableCoreFilename);
    seer.setProjectFilename(projectFilename);

    seer.launchExecutable(launchMode, breakMode);

    seer.show();
    seer.activateWindow();

    return app.exec();
}

