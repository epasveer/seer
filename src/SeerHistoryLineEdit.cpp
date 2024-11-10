#include "SeerHistoryLineEdit.h"

SeerHistoryLineEdit::SeerHistoryLineEdit (const QString& contents, QWidget* parent) : QHistoryLineEdit(contents, parent) {

    QObject::connect(this,  &QHistoryLineEdit::lostFocus,      this, &QHistoryLineEdit::execute);
}

SeerHistoryLineEdit::SeerHistoryLineEdit (QWidget* parent) : QHistoryLineEdit(parent) {

    QObject::connect(this,  &QHistoryLineEdit::lostFocus,      this, &QHistoryLineEdit::execute);
}

