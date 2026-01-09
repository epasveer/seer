// SPDX-FileCopyrightText: 2026 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "SeerMacroToolButton.h"
#include "SeerMacroEditorDialog.h"
#include <QtWidgets/QMenu>
#include <QtWidgets/QMessageBox>
#include <QtGui/QMouseEvent>
#include <QtCore/QTimer>

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

    _macroName = name;
}

const QString& SeerMacroToolButton::macroName () const {

    return _macroName;
}

void SeerMacroToolButton::setCommands (const QStringList& commands) {

    _commands = commands;
}

const QStringList& SeerMacroToolButton::commands () const {

    return _commands;
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
    }
}

