#include "SeerSeerLogWidget.h"
#include "SeerUtl.h"
#include <QtWidgets/QScrollBar>
#include <QRegularExpression>
#include <QtCore/QTime>
#include <QtCore/QDebug>

SeerSeerLogWidget::SeerSeerLogWidget (QWidget* parent) : SeerLogWidget(parent) {
}

SeerSeerLogWidget::~SeerSeerLogWidget () {
}

void SeerSeerLogWidget::processText (const QString& text) {

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

    if (selected == false && text.contains(QRegularExpression("^([0-9]+)\\^")) == true) {
        selected = true;
    }

    if (selected == false && text.contains(QRegularExpression("^([0-9]+)\\*")) == true) {
        selected = true;
    }

    if (selected == false && text.contains(QRegularExpression("^([0-9]+)\\=")) == true) {
        selected = true;
    }

    if (selected == false) {
        return;
    }

    // No filtering.
    QString str = text;

    if (isTimeStampEnabled()) {
        str = QString("[") + QTime::currentTime().toString("hh:mm:ss.zz") + QString("] ") + str;
    }

    // Write the string to the log.
    textEdit->append(str);
}

