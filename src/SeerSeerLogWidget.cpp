#include "SeerSeerLogWidget.h"
#include "SeerUtl.h"
#include <QtWidgets/QScrollBar>
#include <QtCore/QRegExp>
#include <QtCore/QDebug>

SeerSeerLogWidget::SeerSeerLogWidget (QWidget* parent) : SeerLogWidget(parent) {
}

SeerSeerLogWidget::~SeerSeerLogWidget () {
}

void SeerSeerLogWidget::processText (const QString& text) {

    // Don't do anything if we're not enabled.
    if (isLogEnabled() == false) {
        return;
    }

    // Only log '^', '*', and '=' records.
    bool selected = false;

    if (selected == false && text.front() == '^') {
        selected = true;
    }

    if (selected == false && text.front() == '*') {
        selected = true;
    }

    if (selected == false && text.front() == '=') {
        selected = true;
    }

    if (selected == false && text.contains(QRegExp("^([0-9]+)\\^")) == true) {
        selected = true;
    }

    if (selected == false && text.contains(QRegExp("^([0-9]+)\\*")) == true) {
        selected = true;
    }

    if (selected == false && text.contains(QRegExp("^([0-9]+)\\=")) == true) {
        selected = true;
    }

    if (selected == false) {
        return;
    }

    // Filter escape characters.
    QString str = Seer::filterEscapes(text);

    // Write the string to the log.
    textEdit->append(str);

    moveToEnd();
}
