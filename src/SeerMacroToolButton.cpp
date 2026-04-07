// SPDX-FileCopyrightText: 2026 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "SeerMacroToolButton.h"
#include "SeerMacroEditorDialog.h"
#include <QtWidgets/QMenu>
#include <QtWidgets/QMessageBox>
#include <QtGui/QMouseEvent>
#include <QtGui/QShortcut>
#include <QtCore/QTimer>
#include <QtCore/QSettings>
#include <QtCore/QFileInfo>
#include <QtCore/QDir>
#include <QtCore/QFile>
#include <QtCore/QTextStream>

SeerMacroToolButton::SeerMacroToolButton(QWidget* parent) : QToolButton(parent) {

    _holdTimer = new QTimer(this);
    _holdTimer->setSingleShot(true);
    _holdTimer->setInterval(500); // 500ms hold time

    // Create the menu
    _menu = new QMenu(this);
    _menu->addAction("Edit Macro", this, &SeerMacroToolButton::handleEditMacro);

    connect(_holdTimer, &QTimer::timeout,            this, &SeerMacroToolButton::handleHoldTriggered);
}

void SeerMacroToolButton::setMacroName (const QString& name) {

    // Set the macro name. If it's "", nullify our state.
    _macroName = name;

    if (_macroName == "") {
        _macroFileName = "";
        return;
    }

    // Add combination shortcut for re-open closed file (Ctrl + Shift + T)
    QShortcut* shortcut = new QShortcut(QKeySequence(QString("Ctrl+Shift+")+_macroName[1]), this);

    QObject::connect(shortcut, &QShortcut::activated,       this, &QToolButton::click);

    // Create the macro filename where the macro is to be written.
    QSettings settings;

    QFileInfo fileInfo(settings.fileName());

    _macroFileName = fileInfo.absolutePath() + "/macros/" + _macroName + ".macro";

    // Read settings now we have a proper macro name.
    readMacro();
}

const QString& SeerMacroToolButton::macroName () const {

    return _macroName;
}

const QString& SeerMacroToolButton::macroFileName () const {

    return _macroFileName;
}

void SeerMacroToolButton::setCommands (const QStringList& commands) {

    _commands = commands;
}

const QStringList& SeerMacroToolButton::commands () const {

    return _commands;
}

void SeerMacroToolButton::writeMacro () {

    // No macro filename, don't save.
    if (macroFileName() == "") {
        return;
    }

    // Get the absolute path of the filename.
    QFileInfo fileInfo(macroFileName());

    QString path = fileInfo.absolutePath();

    // Create the directory.
    QDir().mkpath(path);

    // Create the macro file and write the lines to it.
    QFile file(macroFileName());

    if (file.open(QFile::WriteOnly|QFile::Text|QFile::Truncate)) {

        QTextStream out(&file);

        for (const QString& line : commands()) {
            out << line << "\n";
        }

        file.close();
    }
}

void SeerMacroToolButton::readMacro () {

    if (macroFileName() == "") {
        return;
    }

    QFile file(macroFileName());

    if (file.open(QFile::ReadOnly|QFile::Text)) {

        QStringList lines;

        QTextStream in(&file);

        while (!in.atEnd()) {
            QString line = in.readLine();
            lines.append(line);
        }

        file.close();

        setCommands(lines);
    }
}

void SeerMacroToolButton::mousePressEvent (QMouseEvent* event) {

    QToolButton::mousePressEvent(event);

    _pressPos = event->pos();
    _holdTimer->start();
}

void SeerMacroToolButton::mouseReleaseEvent (QMouseEvent* event) {

    QToolButton::mouseReleaseEvent(event);

    _holdTimer->stop();
}

void SeerMacroToolButton::handleHoldTriggered () {

    // Show menu at button position
    QPoint globalPos = mapToGlobal(QPoint(0, height()));

    _menu->exec(globalPos);

    // Reset button to unpressed state
    setDown(false);
}

void SeerMacroToolButton::handleEditMacro () {

    SeerMacroEditorDialog dlg;

    dlg.setMacroName(macroName());
    dlg.setCommands(commands());

    if (dlg.exec()) {
        setCommands(dlg.commands());
        writeMacro();
    }
}

