#pragma once

#include <QtWidgets/QPlainTextEdit>
#include <QtWidgets/QWidget>
#include <QtCore/QString>
#include <QtCore/QEvent>
#include <QtCore/QObject>

class SeerPlainTextEdit : public QPlainTextEdit {

    Q_OBJECT

    public:
        explicit SeerPlainTextEdit (const QString& text, QWidget* parent = 0);
        explicit SeerPlainTextEdit (QWidget* parent = 0);
       ~SeerPlainTextEdit ();

        void                    forwardViewportEvent        (QEvent* event);
};


class SeerPlainTextWheelEventForwarder : public QObject {

    Q_OBJECT

    public:
        explicit SeerPlainTextWheelEventForwarder (SeerPlainTextEdit* target);
       ~SeerPlainTextWheelEventForwarder();

        bool                    eventFilter                 (QObject* obj, QEvent* event);

    private:
        SeerPlainTextEdit*      _target;
};

