// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "SeerSignalValuesBrowserWidget.h"
#include "SeerUtl.h"
#include <QtWidgets/QMenu>
#include <QtWidgets/QApplication>
#include <QtCore/QDebug>


SeerSignalValuesBrowserWidget::SeerSignalValuesBrowserWidget (QWidget* parent) : QWidget(parent) {

    // Construct the UI.
    setupUi(this);

    // Setup the widgets
    signalsTreeWidget->resizeColumnToContents(0); // name
    signalsTreeWidget->resizeColumnToContents(1); // stop
    signalsTreeWidget->resizeColumnToContents(2); // print
    signalsTreeWidget->resizeColumnToContents(3); // pass
    signalsTreeWidget->resizeColumnToContents(4); // description
    signalsTreeWidget->clear();

    // Preference menu.
    QMenu* menu = new QMenu();

    menu->addAction("New profile");
    menu->addAction("Modify current profile");
    menu->addAction("Delete current profile");

    preferencesToolButton->setMenu(menu);
    preferencesToolButton->setPopupMode(QToolButton::InstantPopup);

    // Connect things.
}

SeerSignalValuesBrowserWidget::~SeerSignalValuesBrowserWidget () {
}

void SeerSignalValuesBrowserWidget::handleText (const QString& text) {

    // Don't do any work if the widget is hidden.
    if (isHidden()) {
        return;
    }

    QApplication::setOverrideCursor(Qt::BusyCursor);

    if (text.startsWith("^done,signal-values=[") && text.endsWith("]")) {

        signalsTreeWidget->clear();

        // ^done,signal-values=[
        //                  {name="SIGHUP",stop="Yes",print="Yes",pass="Yes",description="Hangup"},
        //                  {name="SIGINT",stop="Yes",print="Yes",pass="No",description="Interrupt"},
        //                  {name="SIGQUIT",stop="Yes",print="Yes",pass="Yes",description="Quit"},
        //                  {name="SIGILL",stop="Yes",print="Yes",pass="Yes",description="Illegal instruction"},
        //                  {name="EXC_EMULATION",stop="Yes",print="Yes",pass="Yes",description="Emulation instruction"},
        //                  {name="EXC_SOFTWARE",stop="Yes",print="Yes",pass="Yes",description="Software generated exception"},
        //                  {name="EXC_BREAKPOINT",stop="Yes",print="Yes",pass="Yes",description="Breakpoint"},
        //                  {name="SIGLIBRT",stop="No",print="No",pass="Yes",description="librt internal signal"}
        //               ]
        //
        // ^done,signal-names=[
        //                  "SIGHUP","SIGINT","SIGQUIT","SIGILL","SIGTRAP","SIGABRT","SIGEMT","SIGFPE","SIGKILL","SIGBUS","SIGSEGV"
        //               ]

        QString signals_text = Seer::parseFirst(text, "signal-values=", '[', ']', false);

        QStringList signals_list = Seer::parse(signals_text, "", '{', '}', false);

        for (const auto& signal_entry : signals_list) {

            QString name_text        = Seer::parseFirst(signal_entry, "name=",        '"', '"', false);
            QString stop_text        = Seer::parseFirst(signal_entry, "stop=",        '"', '"', false);
            QString print_text       = Seer::parseFirst(signal_entry, "print=",       '"', '"', false);
            QString pass_text        = Seer::parseFirst(signal_entry, "pass=",        '"', '"', false);
            QString description_text = Seer::parseFirst(signal_entry, "description=", '"', '"', false);

            // Add the function to the tree.
            QTreeWidgetItem* item = new QTreeWidgetItem;

            item->setText(0, name_text);
            item->setText(1, stop_text);
            item->setText(2, print_text);
            item->setText(3, pass_text);
            item->setText(4, description_text);

            signalsTreeWidget->addTopLevelItem(item);
        }

    }else{
        // Ignore others.
    }

    signalsTreeWidget->resizeColumnToContents(0);
    signalsTreeWidget->resizeColumnToContents(1);
    signalsTreeWidget->resizeColumnToContents(2);
    signalsTreeWidget->resizeColumnToContents(3);
    signalsTreeWidget->resizeColumnToContents(4);

    QApplication::restoreOverrideCursor();
}

void SeerSignalValuesBrowserWidget::handleStoppingPointReached () {

    // Don't do any work if the widget is hidden.
    if (isHidden()) {
        return;
    }

    refresh();
}

void SeerSignalValuesBrowserWidget::handleSessionTerminated () {

    // Delete previous contents.
    signalsTreeWidget->clear();
}

void SeerSignalValuesBrowserWidget::refresh () {

    emit refreshSignalValues();
}

void SeerSignalValuesBrowserWidget::showEvent (QShowEvent* event) {

    QWidget::showEvent(event);

    refresh();
}

