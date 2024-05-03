#include "SeerGdbLogWidget.h"
#include "SeerUtl.h"
#include <QtWidgets/QScrollBar>
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
    if (text.front() == '~') {
        str = text.mid(1);

        // Remove leading """
        if (str.front() == '"') {
            str = str.mid(1);
        }

        // Remove trailing """
        if (str.back() == '"') {
            str.chop(1);
        }

    // Remove leading "&"
    // &"p name
    // "
    } else if (text.front() == '&') {

        str = text.mid(1);

        // Remove leading """
        if (str.front() == '"') {
            str = str.mid(1);
        }

        // Remove trailing """
        if (str.back() == '"') {
            str.chop(1);
        }

    // Remove leading "@"
    // @"memcheck monitor commands:
    // "
    } else if (text.front() == '@') {

        str = text.mid(1);

        // Remove leading """
        if (str.front() == '"') {
            str = str.mid(1);
        }

        // Remove trailing """
        if (str.back() == '"') {
            str.chop(1);
        }

    // Use string as it is.
    }else{
        str = text;
    }

    // Filter out \n and \t.
    // Should probably do something better and expand them.
    str.replace("\\t",  "\t");
    str.replace("\\n",  "\n");
    str.replace("\\\"", "\"");

    // Write the string to the log.
    textEdit->insertPlainText(str);

    // If there is breakpoint message (via a manual command), ask
    // for the breakpoint list to be refreshed.
    //
    // Breakpoint 2 at 0x403a40: file explorer.cpp, line 78.
    //
    if (str.contains(QRegularExpression("^Breakpoint ([0-9]+) at (0[xX][0-9a-fA-F]+): file (.*\\,) (line) ([0-9]+)"))) {
        emit refreshBreakpointsList();
    }
}

