#include "SeerSeerLogWidget.h"
#include "SeerUtl.h"
#include <QtWidgets/QScrollBar>
#include <QRegularExpression>
#include <QtCore/QDebug>

SeerSeerLogWidget::SeerSeerLogWidget (QWidget* parent) : SeerLogWidget(parent) {
}

SeerSeerLogWidget::~SeerSeerLogWidget () {
}

void SeerSeerLogWidget::processText (const QString& text) {

    // Only log '^', '*', and '=' records.
    bool selected = false;

#if QT_VERSION >= QT_VERSION_CHECK(5, 10, 0)
    if (selected == false && text.front() == '^') {
#else
    if (selected == false && text.at(0) == '^') {
#endif
        selected = true;
    }

#if QT_VERSION >= QT_VERSION_CHECK(5, 10, 0)
    if (selected == false && text.front() == '*') {
#else
    if (selected == false && text.at(0) == '*') {
#endif
        selected = true;
    }

#if QT_VERSION >= QT_VERSION_CHECK(5, 10, 0)
    if (selected == false && text.front() == '=') {
#else
    if (selected == false && text.at(0) == '=') {
#endif
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

    // Filter escape characters.
    QString str = Seer::filterEscapes(text);

    // Write the string to the log.
    textEdit->append(str);
}

