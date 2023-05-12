#include "SeerGdbLogWidget.h"
#include "SeerUtl.h"
#include <QtWidgets/QScrollBar>
#include <QtCore5Compat/QRegExp>
#include <QRegularExpression>
#include <QtCore/QDebug>

SeerGdbLogWidget::SeerGdbLogWidget (QWidget* parent) : SeerLogWidget(parent) {
}

SeerGdbLogWidget::~SeerGdbLogWidget () {
}

void SeerGdbLogWidget::processText (const QString& text) {

    QString str;

    // Remove leading "~"
    // ~"For help, type "help".
    // "
#if QT_VERSION >= QT_VERSION_CHECK(5, 10, 0)
    if (text.front() == '~') {
#else
    if (text.at(0) == '~') {
#endif
        str = text.mid(1);

        // Remove leading """
#if QT_VERSION >= QT_VERSION_CHECK(5, 10, 0)
        if (str.front() == '"') {
#else
        if (str.at(0) == '"') {
#endif
            str = str.mid(1);
        }

        // Remove trailing """
#if QT_VERSION >= QT_VERSION_CHECK(5, 10, 0)
        if (str.back() == '"') {
#else
        if (str.at(str.size() - 1) == '"') {
#endif

            str.chop(1);
        }

        // Remove trailing "\n"
#if QT_VERSION >= QT_VERSION_CHECK(5, 10, 0)
        if (str.back() == '\n') {
#else
        if (str.at(str.size() - 1) == '\n') {
#endif
            str.chop(1);
        }

    // Remove leading "&"
    // &"p name
    // "
#if QT_VERSION >= QT_VERSION_CHECK(5, 10, 0)
    } else if (text.front() == '&') {
#else
    } else if (text.at(0) == '&') {
#endif

        str = text.mid(1);

        // Remove leading """
#if QT_VERSION >= QT_VERSION_CHECK(5, 10, 0)
        if (str.front() == '"') {
#else
        if (str.at(0) == '"') {
#endif
            str = str.mid(1);
        }

        // Remove trailing """
#if QT_VERSION >= QT_VERSION_CHECK(5, 10, 0)
        if (str.back() == '"') {
#else
        if (str.at(str.size() - 1) == '"') {
#endif
            str.chop(1);
        }

        // Remove trailing "\n"
#if QT_VERSION >= QT_VERSION_CHECK(5, 10, 0)
        if (str.back() == '\n') {
#else
        if (str.at(str.size() - 1) == '\n') {
#endif
            str.chop(1);
        }


    // Remove leading "@"
    // @"memcheck monitor commands:
    // "
#if QT_VERSION >= QT_VERSION_CHECK(5, 10, 0)
    } else if (text.front() == '@') {
#else
    } else if (text.at(0) == '@') {
#endif

        str = text.mid(1);

        // Remove leading """
#if QT_VERSION >= QT_VERSION_CHECK(5, 10, 0)
        if (str.front() == '"') {
#else
        if (str.at(0) == '"') {
#endif
            str = str.mid(1);
        }

        // Remove trailing """
#if QT_VERSION >= QT_VERSION_CHECK(5, 10, 0)
        if (str.back() == '"') {
#else
        if (str.at(str.size() - 1) == '"') {
#endif
            str.chop(1);
        }

        // Remove trailing "\n"
#if QT_VERSION >= QT_VERSION_CHECK(5, 10, 0)
        if (str.back() == '\n') {
#else
        if (str.at(str.size() - 1) == '\n') {
#endif
            str.chop(1);
        }


    // Use string as it is.
    }else{
        str = text;
    }

    // Filter escape characters.
    str = Seer::filterEscapes(str);

    // Remove trailing "\n"
#if QT_VERSION >= QT_VERSION_CHECK(5, 10, 0)
    while (str.back() == '\n') {
#else
    while (str.at(str.size() - 1) == '\n') {
#endif
        str.chop(1);
        break;
    }

    // Write the string to the log.
    textEdit->append(str);

    // If there is breakpoint message (via a manual command), ask
    // for the breakpoint list to be refreshed.
    //
    // Breakpoint 2 at 0x403a40: file explorer.cpp, line 78.
    //
    if (str.contains(QRegularExpression("^Breakpoint ([0-9]+) at (0[xX][0-9a-fA-F]+): file (.*\\,) (line) ([0-9]+)"))) {
        emit refreshBreakpointsList();
    }
}

