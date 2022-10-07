#include "SeerTildeLogWidget.h"
#include "SeerUtl.h"
#include <QtWidgets/QScrollBar>

SeerTildeLogWidget::SeerTildeLogWidget (QWidget* parent) : SeerLogWidget(parent) {
}

SeerTildeLogWidget::~SeerTildeLogWidget () {
}

void SeerTildeLogWidget::processText (const QString& text) {

    QString str = text.mid(1); // Remove leading "~"

#if QT_VERSION >= QT_VERSION_CHECK(5, 10, 0)
    if (str.front() == '"') { // Remove leading """
#else
    if (str.at(0) == '"') { // Remove leading """
#endif
        str = str.mid(1);
    }

#if QT_VERSION >= QT_VERSION_CHECK(5, 10, 0)
    if (str.back() == '"') { // Remove trailing """
#else
    if (str.at(str.size() - 1) == '"') { // Remove trailing """
#endif
        str.chop(1);
    }

    str = Seer::filterEscapes(str);

#if QT_VERSION >= QT_VERSION_CHECK(5, 10, 0)
    if (str.back() == '\n') { // Remove trailing "\n"
#else
    if (str.at(str.size() - 1) == '\n') { // Remove trailing "\n"
#endif
        str.chop(1);
    }

    textEdit->append(str);
}

