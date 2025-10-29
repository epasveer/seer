#include "SeerRunStatusIndicatorBox.h"
#include <QtWidgets/QApplication>
#include <QGroupBox>
#include <QPainter>
#include <QtWidgets/QMessageBox>

SeerRunStatusIndicatorBox::SeerRunStatusIndicatorBox(QWidget* parent) : QGroupBox(parent) {
    _runStatus = RunStatus::Idle;
    groupBox = new QGroupBox();
    groupBox->setTitle("");
    // --- Create two fixed-size labels ---
    coreLabel = new QLabel("Core");
    coreLabel->setAlignment(Qt::AlignCenter);
    coreLabel->setFixedSize(80, 25);
    coreLabel->setStyleSheet("background-color: lightgray; color: black; font-weight: bold;");

    statusLabel = new QLabel("Status");
    statusLabel->setAlignment(Qt::AlignCenter);
    statusLabel->setFixedSize(150, 25);
    statusLabel->setStyleSheet("background-color: lightgray; color: black; font-weight: bold;");

    // --- Layout with spacing ---
    auto *layout = new QHBoxLayout();
    layout->setContentsMargins(0, 0, 0, 0);
    layout->setSpacing(0);
    layout->addWidget(coreLabel);
    layout->addWidget(statusLabel);
    groupBox->setLayout(layout);
}

SeerRunStatusIndicatorBox::~SeerRunStatusIndicatorBox() {
}

void SeerRunStatusIndicatorBox::setRunStatus (SeerRunStatusIndicatorBox::RunStatus status) {

    // If the status is already set, don't set it again.
    if (status == _runStatus) {
        return;
    }

    // Change the status to the new status.
    _runStatus = status;

    if (status == RunStatus::Idle) {
        statusLabel->setText("Idle");
        statusLabel->setStyleSheet("background-color: lightgray; color: black; font-weight: bold;");

    }else if (status == RunStatus::Stopped) {
        statusLabel->setText("Stopped");
        statusLabel->setStyleSheet("background-color: red; color: black; font-weight: bold;");

    }
    else if (status == RunStatus::Stop_By_Breakpoint) {
        statusLabel->setText("Stop by breakpoint");
        statusLabel->setStyleSheet("background-color: yellow; color: black; font-weight: bold;");
    }
    else if (status == RunStatus::Running) {
        statusLabel->setText("Running");
        statusLabel->setStyleSheet("background-color: green; color: black; font-weight: bold;");

    }
    else if (status == RunStatus::Disconnect) {
        statusLabel->setText("DISCONNECTED");
        statusLabel->setStyleSheet("background-color: red; color: black; font-weight: bold;");

    }
    else {
        statusLabel->setText("Unknown");
    }
    // QMessageBox::warning(this, "SeerRunStatusIndicatorBox", "SeerRunStatusIndicatorBox");
    emit statusChanged(status);
}

SeerRunStatusIndicatorBox::RunStatus SeerRunStatusIndicatorBox::runStatus () const {
    return _runStatus;
}

void SeerRunStatusIndicatorBox::handleText (const QString& text) {

    if (text.startsWith("*running,thread-id=\"")) {

        // *running,thread-id="all"
        // *running,thread-id="2"
        setRunStatus(SeerRunStatusIndicatorBox::Running);

    }else if (text.startsWith("*stopped")) {
        if (text.startsWith("*stopped,reason=\"breakpoint-hit\""))
            setRunStatus(SeerRunStatusIndicatorBox::Stop_By_Breakpoint);
        else
            //^connected,frame={level=\"0\",addr=\"0x00007f48351f80c1\",func=\"read\",args=[],from=\"/lib64/libc.so.6\",arch=\"i386:x86-64\"}"
            setRunStatus(SeerRunStatusIndicatorBox::Stopped);

    }else if (text.startsWith("^done,stack")) {
        // When finish command is invoked
        setRunStatus(SeerRunStatusIndicatorBox::Stopped);

    }else if (text.startsWith("=thread-exited")) {

        //=thread-exited,id="1",group-id="i1"
        setRunStatus(SeerRunStatusIndicatorBox::Stopped);

    }else if (text.startsWith("=thread-group-exited")) {

        //=thread-group-exited,id="i1"
        setRunStatus(SeerRunStatusIndicatorBox::Stopped);

    }else{
        // All other text is ignored by this widget.
        // qDebug() << text;
    }
}

QGroupBox* SeerRunStatusIndicatorBox::indicatorBox() {
    return groupBox;
}

void SeerRunStatusIndicatorBox::setCore(int coreIdx)
{
    coreLabel->setText(QString::number(coreIdx));
    coreLabel->setStyleSheet("background-color: lightgray; color: black; font-weight: bold;");
}

// handle when program stop/ killed
void SeerRunStatusIndicatorBox::handleTerminate() {
    coreLabel->setText("Core");
    coreLabel->setStyleSheet("background-color: lightgray; color: black; font-weight: bold;");
    statusLabel->setText("Status");
    statusLabel->setStyleSheet("background-color: lightgray; color: black; font-weight: bold;");
    // also tell SeerProgressIndicator to stop spinning
    setRunStatus(SeerRunStatusIndicatorBox::Stopped);
}