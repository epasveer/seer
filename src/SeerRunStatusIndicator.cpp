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

    }else if (text.startsWith("*stopped,reason=\"breakpoint-hit\"")) {
        setRunStatus(SeerRunStatusIndicator::Stopped);

    }else if (text.startsWith("*stopped,reason=\"watchpoint-trigger\"")) {

        // *stopped,reason="watchpoint-trigger", ...
        setRunStatus(SeerRunStatusIndicator::Stopped);

    }else if (text.startsWith("*stopped,reason=\"read-watchpoint-trigger\"")) {

        // *stopped,reason="read-watchpoint-trigger", ...
        setRunStatus(SeerRunStatusIndicator::Stopped);

    }else if (text.startsWith("*stopped,reason=\"access-watchpoint-trigger\"")) {

        // *stopped,reason="access-watchpoint-trigger", ...
        setRunStatus(SeerRunStatusIndicator::Stopped);

    }else if (text.startsWith("*stopped,reason=\"watchpoint-scope\"")) {

        // "*stopped,reason="watchpoint-scope\",wpnum="3", ...
        setRunStatus(SeerRunStatusIndicator::Stopped);

    }else if (text.startsWith("*stopped,hw-awpt={")) {

        // ??? May need to parse this more. Look at the 'reason'.
        // "*stopped,hw-awpt={number=\"3\",exp=\"j\"},reason=\"access-watchpoint-trigger\",
        setRunStatus(SeerRunStatusIndicator::Stopped);

    }else if (text.startsWith("*stopped,reason=\"end-stepping-range\"")) {
        setRunStatus(SeerRunStatusIndicator::Stopped);

    }else if (text.startsWith("*stopped,reason=\"function-finished\"")) {
        setRunStatus(SeerRunStatusIndicator::Stopped);

    }else if (text.startsWith("*stopped,reason=\"location-reached\"")) {
        setRunStatus(SeerRunStatusIndicator::Stopped);

    }else if (text.startsWith("*stopped,reason=\"signal-received\"")) {
        setRunStatus(SeerRunStatusIndicator::Stopped);

    }else if (text.startsWith("*stopped,frame=")) {
        //*stopped,frame={addr=\"0x00007f0ee0d2d954\",func=\"rfft\",args=[{name=\"a\",value=\"...\"},...
        //*stopped,frame={addr="0x00007f608ec49fc0",func="__pthread_clockjoin_ex",args=[],from="/lib64/libpthread.so.0",arch="i386:x86-64"},thread-id="1",stopped-threads="all",core="3"
        //*stopped,frame={addr="0x00007ff831151329",func="cfft",args=[{name="a",value="..."},{name="n",value="512"},{name="iflg",value="1"}],file="sssMathlib.f",fullname="/home/erniep/Development/Peak/src/Core/Math/sssMathlib.f",line="767",arch="i386:x86-64"},thread-id="1",stopped-threads="all",core="3"
        setRunStatus(SeerRunStatusIndicator::Stopped);

    }else if (text.startsWith("*stopped,reason=\"exited-normally\"")) {
        setRunStatus(SeerRunStatusIndicator::Idle);

    }else if (text.startsWith("*stopped,reason=\"exited-signalled\"")) {

        // "*stopped,reason=\"exited-signalled\",signal-name=\"SIGSEGV\",signal-meaning=\"Segmentation fault\""
        setRunStatus(SeerRunStatusIndicator::Idle);

    }else if (text.startsWith("*stopped,reason=\"exited\"")) {
        setRunStatus(SeerRunStatusIndicator::Idle);

    }else if (text.startsWith("^connected,frame=")) {
        //^connected,frame={level=\"0\",addr=\"0x00007f48351f80c1\",func=\"read\",args=[],from=\"/lib64/libc.so.6\",arch=\"i386:x86-64\"}"
        return;

    }else{
        // All other text is ignored by this widget.
        qDebug() << __PRETTY_FUNCTION__ << ":" << text;
    }
}

