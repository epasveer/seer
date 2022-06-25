#include "SeerPlainTextEdit.h"

SeerPlainTextEdit::SeerPlainTextEdit(const QString& text, QWidget* parent) : QPlainTextEdit(text, parent) {
}

SeerPlainTextEdit::SeerPlainTextEdit(QWidget* parent) : QPlainTextEdit(parent) {
}

SeerPlainTextEdit::~SeerPlainTextEdit () {
}

void SeerPlainTextEdit::forwardViewportEvent(QEvent* event) {

    viewportEvent(event);
}



SeerPlainTextWheelEventForwarder::SeerPlainTextWheelEventForwarder (SeerPlainTextEdit* target ) : QObject(), _target(target) {
}

SeerPlainTextWheelEventForwarder::~SeerPlainTextWheelEventForwarder() {
    _target = NULL;
}

bool SeerPlainTextWheelEventForwarder::eventFilter (QObject* obj, QEvent* event) {

    Q_UNUSED(obj);

    if (_target == 0) {
        return false;
    }

    if (event->type() == QEvent::Wheel) {
        _target->forwardViewportEvent(event);
    }

    // do not filter the event
    return false;
}

