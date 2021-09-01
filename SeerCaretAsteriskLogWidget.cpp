#include "SeerCaretAsteriskLogWidget.h"
#include "SeerUtl.h"
#include <QtWidgets/QScrollBar>
#include <QtCore/QRegExp>
#include <QtCore/QDebug>

SeerCaretAsteriskLogWidget::SeerCaretAsteriskLogWidget (QWidget* parent) : SeerLogWidget(parent) {
}

SeerCaretAsteriskLogWidget::~SeerCaretAsteriskLogWidget () {
}

void SeerCaretAsteriskLogWidget::processText (const QString& text) {

    // Only log '^' and '*' records.
    if (text.front() != '^' && text.front() != '*' && text.contains(QRegExp("^([0-9]+)\\^")) == false) {
        return;
    }

    // Filter escape characters.
    QString str = Seer::filterEscapes(text);

    // Write the string to the log.
    textEdit->append(str);
    textEdit->verticalScrollBar()->setValue(textEdit->verticalScrollBar()->maximum());
}

