#include "SeerGdbLogWidget.h"
#include "SeerUtl.h"
#include <QtWidgets/QScrollBar>
#include <QtCore/QDebug>

SeerGdbLogWidget::SeerGdbLogWidget (QWidget* parent) : SeerLogWidget(parent) {
}

SeerGdbLogWidget::~SeerGdbLogWidget () {
}

void SeerGdbLogWidget::processText (const QString& text) {

    // Don't do anything if we're not enabled.
    if (isLogEnabled() == false) {
        return;
    }

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

        // Remove trailing "\n"
        if (str.back() == '\n') {
            str.chop(1);
        }

    // Remove leading "&"
    // &"p name
    // "
    }else if (text.front() == '&') {

        str = text.mid(1);

        // Remove leading """
        if (str.front() == '"') {
            str = str.mid(1);
        }

        // Remove trailing """
        if (str.back() == '"') {
            str.chop(1);
        }

        // Remove trailing "\n"
        if (str.back() == '\n') {
            str.chop(1);
        }


    // Use string as it is.
    }else{
        str = text;
    }

    // Filter escape characters.
    str = Seer::filterEscapes(str);

    // Remove trailing "\n"
    while (str.back() == '\n') {
        str.chop(1);
        break;
    }

    // Write the string to the log.
    textEdit->append(str);

    moveToEnd();
}

