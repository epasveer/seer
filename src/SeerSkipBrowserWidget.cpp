#include "SeerSkipBrowserWidget.h"
#include "SeerUtl.h"
#include <QtWidgets/QTreeWidget>
#include <QtWidgets/QTreeWidgetItemIterator>
#include <QtWidgets/QLabel>
#include <QtWidgets/QApplication>
#include <QtCore/QFileInfo>
#include <QtCore/Qt>
#include <QtCore/QMap>
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
    QObject::connect(skipDeleteToolButton,  &QToolButton::clicked,      this, &SeerSkipBrowserWidget::handleDeleteToolButton);
    QObject::connect(skipEnableToolButton,  &QToolButton::clicked,      this, &SeerSkipBrowserWidget::handleEnableToolButton);
    QObject::connect(skipDisableToolButton, &QToolButton::clicked,      this, &SeerSkipBrowserWidget::handleDisableToolButton);
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

    if (ids == "") return;

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

    if (ids == "") return;

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

    if (ids == "") return;

    // Send the list of ID's to disable.
    emit disableSkips(ids);
}

void SeerSkipBrowserWidget::refresh () {
    emit refreshSkipList();
}

