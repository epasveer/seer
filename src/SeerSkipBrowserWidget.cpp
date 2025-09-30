// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "SeerSkipBrowserWidget.h"
#include "SeerSkipCreateDialog.h"
#include "SeerUtl.h"
#include <QtWidgets/QTreeWidget>
#include <QtWidgets/QTreeWidgetItemIterator>
#include <QtWidgets/QMessageBox>
#include <QtCore/QSettings>
#include <QtCore/QDebug>

SeerSkipBrowserWidget::SeerSkipBrowserWidget (QWidget* parent) : QWidget(parent) {

    // Construct the UI.
    setupUi(this);

    // Setup the widgets
    skipTreeWidget->setMouseTracking(true);
    skipTreeWidget->resizeColumnToContents(0);
    skipTreeWidget->resizeColumnToContents(1);
    skipTreeWidget->resizeColumnToContents(2);
    skipTreeWidget->resizeColumnToContents(3);
    skipTreeWidget->resizeColumnToContents(4);
    skipTreeWidget->resizeColumnToContents(5);
    skipTreeWidget->clear();
    skipTreeWidget->setSortingEnabled(false);

    // Connect things.
    QObject::connect(skipAddToolButton,     &QToolButton::clicked,      this, &SeerSkipBrowserWidget::handleAddToolButton);
    QObject::connect(skipDeleteToolButton,  &QToolButton::clicked,      this, &SeerSkipBrowserWidget::handleDeleteToolButton);
    QObject::connect(skipEnableToolButton,  &QToolButton::clicked,      this, &SeerSkipBrowserWidget::handleEnableToolButton);
    QObject::connect(skipDisableToolButton, &QToolButton::clicked,      this, &SeerSkipBrowserWidget::handleDisableToolButton);
    QObject::connect(skipSaveToolButton,    &QToolButton::clicked,      this, &SeerSkipBrowserWidget::handleSaveToolButton);
    QObject::connect(skipLoadToolButton,    &QToolButton::clicked,      this, &SeerSkipBrowserWidget::handleLoadToolButton);
}

SeerSkipBrowserWidget::~SeerSkipBrowserWidget () {
}

void SeerSkipBrowserWidget::handleText (const QString& text) {

    QApplication::setOverrideCursor(Qt::BusyCursor);

    if (text.startsWith("^done,skips=[") && text.endsWith("]")) {

        skipTreeWidget->clear();
        skipTreeWidget->setSortingEnabled(false);
        skipTreeWidget->sortByColumn(-1, Qt::AscendingOrder);

        // ^done,skips=[
        //              {number="1",enable="y",glob="n",file="<none>",re="n",function="std::vector<int, std::allocator<int> >::back()"},
        //              {number="2",enable="y",glob="n",file="<none>",re="n",function="std::unique_ptr<int, std::default_delete<int> >::operator*() const"}
        //             ]

        QString skips_text = Seer::parseFirst(text, "skips=", '[', ']', false);

        QStringList skips_list = Seer::parse(skips_text, "", '{', '}', false);

        for (const auto& skip_entry : skips_list) {

            QString number_text   = Seer::parseFirst(skip_entry, "number=",   '"', '"', false);
            QString enable_text   = Seer::parseFirst(skip_entry, "enable=",   '"', '"', false);
            QString glob_text     = Seer::parseFirst(skip_entry, "glob=",     '"', '"', false);
            QString file_text     = Seer::parseFirst(skip_entry, "file=",     '"', '"', false);
            QString re_text       = Seer::parseFirst(skip_entry, "re=",       '"', '"', false);
            QString function_text = Seer::parseFirst(skip_entry, "function=", '"', '"', false);

            // Add the function to the tree.
            QTreeWidgetItem* item = new QTreeWidgetItem;

            item->setText(0, number_text);
            item->setText(1, enable_text);
            item->setText(2, glob_text);
            item->setText(3, file_text);
            item->setText(4, re_text);
            item->setText(5, function_text);

            skipTreeWidget->addTopLevelItem(item);
        }

    }else{
        // Ignore others.
    }

    skipTreeWidget->resizeColumnToContents(0);
    skipTreeWidget->resizeColumnToContents(1);
    skipTreeWidget->resizeColumnToContents(2);
    skipTreeWidget->resizeColumnToContents(3);
    skipTreeWidget->resizeColumnToContents(4);
    skipTreeWidget->resizeColumnToContents(5);

    QApplication::restoreOverrideCursor();
}

void SeerSkipBrowserWidget::handleSessionTerminated () {

    // Delete previous contents.
    skipTreeWidget->clear();
}

void SeerSkipBrowserWidget::handleAddToolButton () {

    // Create the dialog.
    SeerSkipCreateDialog dialog(this);

    // Execute it.
    if (dialog.exec() != QDialog::Accepted) {
        return;
    }

    // Get result.
    QString mode       = dialog.skipMode();
    QString parameters = dialog.skipParameters();

    if (mode == "" || parameters == "") {
        return;
    }

    // Send the 'add skip' command.
    emit addSkip(mode, parameters);
}

void SeerSkipBrowserWidget::handleDeleteToolButton () {

    // Get selected tree items.
    QList<QTreeWidgetItem*> items = skipTreeWidget->selectedItems();

    // Build ID list.
    QString ids;

    for (int i=0; i<items.size(); i++) {
        if (i != 0) {
            ids += " ";
        }
        ids += items[i]->text(0);
    }

    // Send the list of ID's to delete.
    emit deleteSkips(ids);
}

void SeerSkipBrowserWidget::handleEnableToolButton () {

    // Get selected tree items.
    QList<QTreeWidgetItem*> items = skipTreeWidget->selectedItems();

    // Build ID list.
    QString ids;

    for (int i=0; i<items.size(); i++) {
        if (i != 0) {
            ids += " ";
        }
        ids += items[i]->text(0);
    }

    // Send the list of ID's to enable.
    emit enableSkips(ids);
}

void SeerSkipBrowserWidget::handleDisableToolButton () {

    // Get selected tree items.
    QList<QTreeWidgetItem*> items = skipTreeWidget->selectedItems();

    // Build ID list.
    QString ids;

    for (int i=0; i<items.size(); i++) {
        if (i != 0) {
            ids += " ";
        }
        ids += items[i]->text(0);
    }

    // Send the list of ID's to disable.
    emit disableSkips(ids);
}

void SeerSkipBrowserWidget::handleSaveToolButton () {

    int result = QMessageBox::warning(this, "Seer",
            QString("Save the skips in the view?\n\nPreviously saved skips will be deleted first."),
            QMessageBox::Ok|QMessageBox::Cancel, QMessageBox::Cancel);

    if (result == QMessageBox::Cancel) return;

    QSettings settings;

    settings.beginWriteArray("gdbskips"); {
        int i=0;
        QTreeWidgetItemIterator it(skipTreeWidget);
        while (*it) {
            settings.setArrayIndex(i);
            settings.setValue("number",   (*it)->text(0));
            settings.setValue("enable",   (*it)->text(1));
            settings.setValue("glob",     (*it)->text(2));
            settings.setValue("file",     (*it)->text(3));
            settings.setValue("re",       (*it)->text(4));
            settings.setValue("function", (*it)->text(5));
            ++i;
            ++it;
        }
    } settings.endArray();
}

void SeerSkipBrowserWidget::handleLoadToolButton () {

    // Warn only if there are existing skips in the view.
    if (skipTreeWidget->topLevelItemCount() > 0) {
        int result = QMessageBox::warning(this, "Seer",
                QString("Load the previously saved skips from settings?\n\nThe existing skips in the view will be deleted first."),
                QMessageBox::Ok|QMessageBox::Cancel, QMessageBox::Cancel);

        if (result == QMessageBox::Cancel) return;
    }

    QSettings settings;

    if (settings.childGroups().contains("gdbskips")) {

        emit deleteSkips(""); // Tell gdb to delete all skips. This will update the view.

        int size = settings.beginReadArray("gdbskips");

        for (int i = 0; i < size; ++i) {

            settings.setArrayIndex(i);

            QString number   = settings.value("number").toString();
            QString enable   = settings.value("enable").toString();
            QString glob     = settings.value("glob").toString();
            QString file     = settings.value("file").toString();
            QString re       = settings.value("re").toString();
            QString function = settings.value("function").toString();

            QString mode;
            QString parameters;

            // Work on file mode.
            if (file != "<none>") {
                if (glob == "y") {
                    parameters = file;
                    mode       = "gfile";
                }else if (glob == "n") {
                    parameters = file;
                    mode       = "file";
                }

            // Work on function mode.
            } else if (function != "<none>") {
                if (re == "y") {
                    parameters = function;
                    mode       = "rfunction";
                }else if (re == "n") {
                    parameters = function;
                    mode       = "function";
                }else{
                    continue;
                }
            }

            if (mode == "" || parameters == "") continue;

            emit addSkip(mode, parameters);

        } settings.endArray();
    }
}

void SeerSkipBrowserWidget::refresh () {
    emit refreshSkipList();
}

