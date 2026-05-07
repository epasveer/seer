// SPDX-FileCopyrightText: 2026 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "SeerGdbMonitorWidget.h"
#include "SeerHelpPageDialog.h"
#include "SeerUtl.h"
#include <QtWidgets/QMessageBox>
#include <QtWidgets/QFileDialog>
#include <QtWidgets/QFontDialog>
#include <QtPrintSupport/QPrinter>
#include <QtPrintSupport/QPrintDialog>
#include <QtGui/QFont>
#include <QtCore/QTextStream>
#include <QtCore/QRegularExpression>
#include <QtCore/QSettings>
#include <QtCore/QDebug>

SeerGdbMonitorWidget::SeerGdbMonitorWidget (QWidget* parent) : QWidget(parent) {

    // Init variables.
    _monitorId = Seer::createID(); // Create id for gdb monitor messages.

    // Set up UI.
    setupUi(this);

    // Setup the widgets
    setWindowIcon(QIcon(":/seer/resources/icons/hicolor/64x64/seergdb.png"));
    setWindowTitle("Seer - GDB Monitor");
    setAttribute(Qt::WA_DeleteOnClose);

    m1ToolButton->setMacroName("M1", "gdbmonitor");
    m2ToolButton->setMacroName("M2", "gdbmonitor");
    m3ToolButton->setMacroName("M3", "gdbmonitor");
    m4ToolButton->setMacroName("M4", "gdbmonitor");
    m5ToolButton->setMacroName("M5", "gdbmonitor");
    m6ToolButton->setMacroName("M6", "gdbmonitor");
    m7ToolButton->setMacroName("M7", "gdbmonitor");
    m8ToolButton->setMacroName("M8", "gdbmonitor");
    m9ToolButton->setMacroName("M9", "gdbmonitor");
    m0ToolButton->setMacroName("M0", "gdbmonitor");

    // Default font.
    QFont font;
    font.setFamily("monospace");
    font.setStyleHint(QFont::Monospace);
    font.setFixedPitch(true);

    // Set the widget's font.
    QTextCharFormat format = textEdit->currentCharFormat();
    format.setFont(font);
    textEdit->setCurrentCharFormat(format);
    textEdit->setFont(font);

    // Connect things.
    QObject::connect(monitorCommandLineEdit,        &QLineEdit::returnPressed,                                 this,            &SeerGdbMonitorWidget::handleCommandLineEdit);
    QObject::connect(clearToolButton,               &QToolButton::clicked,                                     this,            &SeerGdbMonitorWidget::handleClearButton);
    QObject::connect(saveToolButton,                &QToolButton::clicked,                                     this,            &SeerGdbMonitorWidget::handleSaveButton);
    QObject::connect(printToolButton,               &QToolButton::clicked,                                     this,            &SeerGdbMonitorWidget::handlePrintButton);
    QObject::connect(helpToolButton,                &QToolButton::clicked,                                     this,            &SeerGdbMonitorWidget::handleHelpButton);
    QObject::connect(macroButtonGroup,              &QButtonGroup::buttonClicked,                              this,            &SeerGdbMonitorWidget::handleMacroToolButtonClicked);

    // Restore window settings.
    readSettings();
}

SeerGdbMonitorWidget::~SeerGdbMonitorWidget () {
}

void SeerGdbMonitorWidget::handleText (const QString& text) {

    // qDebug() << text;

    if (text.contains(QRegularExpression("^([0-9]+)\\^done,monitor-output="))) {

        // 13^done,monitor-output=\"The following monitor command\\n\"
        // 13^done,monitor-output=\"\"

        QString id_text = text.section('^', 0,0);

        if (id_text.toInt() == _monitorId) {
            QString output_text = Seer::parseFirst(text, "monitor-output=", '"', '"', false);
            textEdit->appendPlainText(Seer::unescape(output_text));
            textEdit->moveCursor(QTextCursor::End);
        }
    }
}

void SeerGdbMonitorWidget::handleCommandLineEdit () {

    QString command = monitorCommandLineEdit->text();

    monitorCommandLineEdit->clear();

    textEdit->appendPlainText("(monitor) " + command);
    textEdit->moveCursor(QTextCursor::End);

    emit executeGdbMonitorCommand(_monitorId, command);
}

void SeerGdbMonitorWidget::handleClearButton () {

    textEdit->clear();
}

void SeerGdbMonitorWidget::handlePrintButton () {

    QPrinter printer;

    QPrintDialog* dlg = new QPrintDialog(&printer, this);

    if (dlg->exec() != QDialog::Accepted) {
        return;
    }

    QTextDocument* document = textEdit->document();

    document->print(&printer);
}

void SeerGdbMonitorWidget::handleSaveButton () {

    QFileDialog dialog(this, "Seer log file", "./", "Logs (*.log);;Text files (*.txt);;All files (*.*)");
    dialog.setOptions(QFileDialog::DontUseNativeDialog);
    dialog.setAcceptMode(QFileDialog::AcceptSave);
    dialog.setFileMode(QFileDialog::AnyFile);
    dialog.setDefaultSuffix("log");
    dialog.selectFile("gdbmonitor.log");

    if (dialog.exec() != QDialog::Accepted) {
        return;
    }

    QStringList files = dialog.selectedFiles();

    if (files.size() == 0) {
        return;
    }

    if (files.size() > 1) {
        QMessageBox::critical(this, tr("Error"), tr("Select only 1 file."));
        return;
    }

    QFile file(files[0]);

    if (file.open(QIODevice::ReadWrite)) {
        QTextStream stream(&file);
        stream << textEdit->toPlainText();
        file.flush();
        file.close();

    }else{
        QMessageBox::critical(this, tr("Error"), tr("Cannot save log to file."));
        return;
    }
}

void SeerGdbMonitorWidget::handleHelpButton () {

    SeerHelpPageDialog* help = new SeerHelpPageDialog;
    help->loadFile(":/seer/resources/help/GdbMonitor.md");
    help->show();
    help->raise();
}

void SeerGdbMonitorWidget::handleMacroToolButtonClicked (QAbstractButton* button) {

    if (button == nullptr) {
        return;
    }

    SeerMacroToolButton* tb = qobject_cast<SeerMacroToolButton*>(button);

    for (auto& command : tb->commands()) {
        if (command == "") {
            continue;
        }

        textEdit->appendPlainText("(monitor) " + command);
        textEdit->moveCursor(QTextCursor::End);

        emit executeGdbMonitorCommand(_monitorId, command);
    }
}

void SeerGdbMonitorWidget::writeSettings() {

    QSettings settings;

    settings.beginGroup("gdbmonitorvisualizerwindow"); {
        settings.setValue("size", size());
    } settings.endGroup();
}

void SeerGdbMonitorWidget::readSettings() {

    QSettings settings;

    settings.beginGroup("gdbmonitorvisualizerwindow"); {
        resize(settings.value("size", QSize(800, 400)).toSize());
    } settings.endGroup();
}

void SeerGdbMonitorWidget::resizeEvent (QResizeEvent* event) {

    writeSettings();

    QWidget::resizeEvent(event);
}

