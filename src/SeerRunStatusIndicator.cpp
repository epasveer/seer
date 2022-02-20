#include "SeerRunStatusIndicator.h"
#include <QtWidgets/QApplication>
#include <QtCore/QDebug>

SeerRunStatusIndicator::SeerRunStatusIndicator(QWidget* parent) : QLabel(parent) {
    _runStatus = RunStatus::Idle;
}

SeerRunStatusIndicator::~SeerRunStatusIndicator() {
}

void SeerRunStatusIndicator::setRunStatus (SeerRunStatusIndicator::RunStatus status) {

    // If the status is already set, don't set it again.
    if (status == _runStatus) {
        return;
    }

    // Change the status to the new status.
    _runStatus = status;

    if (status == RunStatus::Idle) {
        QApplication::restoreOverrideCursor();
        setText("Idle");

    }else if (status == RunStatus::Stopped) {
        QApplication::restoreOverrideCursor();
        setText("Stopped");

    }else if (status == RunStatus::Running) {
        QApplication::setOverrideCursor(Qt::BusyCursor);
        setText("Running");

    }else{
        QApplication::restoreOverrideCursor();
        setText("Unknown");
    }

    emit statusChanged(status);
}

SeerRunStatusIndicator::RunStatus SeerRunStatusIndicator::runStatus () const {
    return _runStatus;
}

void SeerRunStatusIndicator::handleText (const QString& text) {

    if (text.startsWith("*running,thread-id=\"")) {

        // *running,thread-id="all"
        // *running,thread-id="2"
        setRunStatus(SeerRunStatusIndicator::Running);

    }else if (text.startsWith("*stopped")) {

        setRunStatus(SeerRunStatusIndicator::Stopped);

        //^connected,frame={level=\"0\",addr=\"0x00007f48351f80c1\",func=\"read\",args=[],from=\"/lib64/libc.so.6\",arch=\"i386:x86-64\"}"
        return;

    }else{
        // All other text is ignored by this widget.
        qDebug() << text;
    }
}

