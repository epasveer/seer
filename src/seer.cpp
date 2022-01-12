#include "SeerMainWindow.h"
#include "SeerUtl.h"
#include <QtWidgets/QApplication>
#include <QtGui/QIcon>
#include <QtCore/QCommandLineParser>
#include <QtCore/QCommandLineOption>
#include <QtCore/QStringList>
#include <QtCore/QString>

int main (int argc, char* argv[]) {

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

    QCommandLineOption startOption(QStringList()<<"s"<<"start", QCoreApplication::translate("main", "Load the executable, break in main(), and run it."));
    parser.addOption(startOption);

    QCommandLineOption attachOption(QStringList()<<"attach", QCoreApplication::translate("main", "Attach to a running process."), "pid");
    parser.addOption(attachOption);

    QCommandLineOption connectOption(QStringList()<<"connect", QCoreApplication::translate("main", "Connect to a running process with gdbserver. Manually start gdbserver first."), "host:port");
    parser.addOption(connectOption);

    QCommandLineOption corefileOption(QStringList()<<"core", QCoreApplication::translate("main", "Load a corefile."), "corefile");
    parser.addOption(corefileOption);


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
    int     executablePid = -1;
    QString executableHostPort;
    QString executableCoreFilename;

    if (parser.isSet(runOption)) {
        launchMode = "run";
    }

    if (parser.isSet(startOption)) {
        launchMode = "start";
    }

    if (parser.isSet(attachOption)) {
        launchMode = "attach";

        QString pid = parser.value(attachOption);

        executablePid = pid.toInt();
    }

    if (parser.isSet(corefileOption)) {
        launchMode = "corefile";

        executableCoreFilename = parser.value(corefileOption);
    }

    if (parser.isSet(connectOption)) {
        launchMode = "connect";

        executableHostPort = parser.value(connectOption);
    }

    //
    // Start Seer
    //
    SeerMainWindow seer;

    seer.setWindowIcon(QIcon(":/seer/resources/seer_64x64.png"));
    seer.setExecutableName(executableName);
    seer.setExecutableArguments(positionalArguments);
    seer.setExecutablePid(executablePid);
    seer.setExecutableHostPort(executableHostPort);
    seer.setExecutableCoreFilename(executableCoreFilename);

    seer.launchExecutable(launchMode);

    seer.move(800, 400);
    seer.show();
    seer.activateWindow();

    return app.exec();
}

