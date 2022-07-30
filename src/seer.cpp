#include "SeerMainWindow.h"
#include "SeerUtl.h"
#include <QtWidgets/QApplication>
#include <QtGui/QIcon>
#include <QtCore/QCommandLineParser>
#include <QtCore/QCommandLineOption>
#include <QtCore/QStringList>
#include <QtCore/QString>
#include <QtCore/QDebug>

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

    QCoreApplication::setApplicationName("Seer");
    QCoreApplication::setOrganizationName("Seer");
    QCoreApplication::setApplicationVersion(Seer::version() + " - Ernie Pasveer (c)2021");

    //
    // Parse arguments.
    //
    QCommandLineParser parser;
    parser.setApplicationDescription("\nSeer - A gui frontend for gdb.");
    parser.setOptionsAfterPositionalArgumentsMode(QCommandLineParser::ParseAsPositionalArguments); // Treat all arguments after the executable name as positional arguments too.
    parser.setSingleDashWordOptionMode(QCommandLineParser::ParseAsLongOptions); // Treat all arguments long options.
    parser.addHelpOption();
    parser.addVersionOption();

    // Run or start options.
    QCommandLineOption runOption(QStringList()<<"r"<<"run", QCoreApplication::translate("main", "Load the executable and run it."));
    parser.addOption(runOption);

    QCommandLineOption startOption(QStringList()<<"s"<<"start", QCoreApplication::translate("main", "Load the executable, break in \"main\", and run it."));
    parser.addOption(startOption);

    QCommandLineOption breakpointsOption(QStringList()<<"b"<<"breakpoints", QCoreApplication::translate("main", "Optionally load a previously saved breakpoints file. For --run or --start"), "filename");
    parser.addOption(breakpointsOption);

    QCommandLineOption attachOption(QStringList()<<"attach", QCoreApplication::translate("main", "Attach to a locally running process."), "pid");
    parser.addOption(attachOption);

    QCommandLineOption connectOption(QStringList()<<"connect", QCoreApplication::translate("main", "Connect to a running process with gdbserver (local or remote). Manually start gdbserver first.\nPossible connection mediums are:\n    host:port\n    /dev/<serialdev>"), "medium");
    parser.addOption(connectOption);

    QCommandLineOption corefileOption(QStringList()<<"core", QCoreApplication::translate("main", "Load a corefile."), "corefile");
    parser.addOption(corefileOption);

    QCommandLineOption configOption(QStringList()<<"config", QCoreApplication::translate("main", "Launch with config dialog.\nSave settings with:\n    'Settings->Save Configuration'"));
    parser.addOption(configOption);


    // A positional argument for executable name.
    // All other arguments after that are treated as positional arguments for the executable.
    parser.addPositionalArgument("executable", QCoreApplication::translate("main", "The executable to debug. Needed for all run modes."));
    parser.addPositionalArgument("arguments",  QCoreApplication::translate("main", "Arguments for the executable. Needed for --run and --start."), "[arguments ...]");

    // Process the arguments.
    parser.process(app);

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
    QString executableHostPort;
    QString executableBreakpointsFilename;
    QString executableCoreFilename;

    if (parser.isSet(runOption)) {
        launchMode = "run";
        breakMode  = "none";
    }

    if (parser.isSet(startOption)) {
        launchMode = "run";
        breakMode  = "inmain";
    }

    if (parser.isSet(breakpointsOption)) {
        executableBreakpointsFilename = parser.value(breakpointsOption);
    }

    if (parser.isSet(attachOption)) {
        launchMode = "attach";

        QString pid = parser.value(attachOption);

        executablePid = pid.toInt();
    }

    if (parser.isSet(connectOption)) {
        launchMode = "connect";

        executableHostPort = parser.value(connectOption);
    }

    if (parser.isSet(corefileOption)) {
        launchMode = "corefile";

        executableCoreFilename = parser.value(corefileOption);
    }

    if (parser.isSet(configOption)) {
        launchMode = "configdialog";
    }


    //
    // Start Seer
    //
    SeerMainWindow seer;

    seer.setWindowIcon(QIcon(":/seer/resources/seer_64x64.png"));
    seer.setExecutableName(executableName);
    seer.setExecutableArguments(positionalArguments);
    seer.setExecutableBreakpointsFilename(executableBreakpointsFilename);
    seer.setExecutablePid(executablePid);
    seer.setExecutableHostPort(executableHostPort);
    seer.setExecutableCoreFilename(executableCoreFilename);

    seer.launchExecutable(launchMode, breakMode);

    seer.move(800, 400);
    seer.show();
    seer.activateWindow();

    return app.exec();
}

