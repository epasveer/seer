#include "SeerSourceConfigPage.h"
#include <QtWidgets/QInputDialog>
#include <QtWidgets/QWidget>
#include <QtCore/QDebug>

SeerSourceConfigPage::SeerSourceConfigPage(QWidget* parent) : QWidget(parent) {

    // Set up the UI.
    setupUi(this);

    // Setup the widgets
    directoriesTreeWidget->clear();
    directoriesTreeWidget->setSortingEnabled(false);
    directoriesTreeWidget->setSelectionMode(QAbstractItemView::ExtendedSelection);
    directoriesTreeWidget->resizeColumnToContents(0); // directory

    // Connect things.
    QObject::connect(addDirectoryToolButton,        &QToolButton::clicked,     this, &SeerSourceConfigPage::handleAddButtonClicked);
    QObject::connect(moveUpDirectoriesToolButton,   &QToolButton::clicked,     this, &SeerSourceConfigPage::handleUpButtonClicked);
    QObject::connect(moveDownDirectoriesToolButton, &QToolButton::clicked,     this, &SeerSourceConfigPage::handleDownButtonClicked);
    QObject::connect(deleteDirectoriesToolButton,   &QToolButton::clicked,     this, &SeerSourceConfigPage::handleDeleteButtonClicked);
}

SeerSourceConfigPage::~SeerSourceConfigPage() {
}

void SeerSourceConfigPage::setAlternateDirectories (const QStringList& alternateDirectories) {

    directoriesTreeWidget->clear();

    QStringListIterator iter(alternateDirectories);

    while (iter.hasNext()) {

        QTreeWidgetItem* topItem = new QTreeWidgetItem;
        topItem->setText(0, iter.next());

        directoriesTreeWidget->addTopLevelItem(topItem);
    }

    directoriesTreeWidget->resizeColumnToContents(0);
}

QStringList SeerSourceConfigPage::alternateDirectories () const {

    QStringList list;

    QTreeWidgetItemIterator iter(directoriesTreeWidget);

    while (*iter) {
        list << (*iter)->text(0);
        ++iter;
    }

    return list;
}

void SeerSourceConfigPage::handleAddButtonClicked () {

    // Ask for the alternate directory to add to the list.
    QString directory = QInputDialog::getText(this, "Seer", "Enter an alternate directory",  QLineEdit::Normal);

    if (directory == "") {
        return;
    }

    // Get the selected items, if any.
    QList<QTreeWidgetItem*> matches = directoriesTreeWidget->selectedItems();

    // Create the new item
    QTreeWidgetItem* topItem = new QTreeWidgetItem;
    topItem->setText(0, directory);

    // If there is something selected, add the new item before the first selected item.
    if (matches.count() > 0) {

        int index = directoriesTreeWidget->indexOfTopLevelItem(matches[0]);

        directoriesTreeWidget->insertTopLevelItem(index, topItem);

    // Otherwise, just add it to the end of the list.
    }else{
        directoriesTreeWidget->addTopLevelItem(topItem);
    }

    // Resize the columns and select the item we just added.
    directoriesTreeWidget->resizeColumnToContents(0);

    directoriesTreeWidget->setCurrentItem(topItem, 0);
}

void SeerSourceConfigPage::handleUpButtonClicked () {

    // Get the selected items.
    QList<QTreeWidgetItem*> matches = directoriesTreeWidget->selectedItems();

    // Loop through them from low to high.
    for (int i=0; i<matches.count(); i++) {

        int index = directoriesTreeWidget->indexOfTopLevelItem(matches[i]);

        // Stop if the item is at the top of the list.
        if (index <= 0) {
            break;
        }

        // Steel it from the treeWidget.
        QTreeWidgetItem* topItem = directoriesTreeWidget->takeTopLevelItem(index);

        // Re-add it to the moved up position.
        directoriesTreeWidget->insertTopLevelItem(index-1, topItem);
    }

    // Re-selected the items.
    directoriesTreeWidget->clearSelection();
    for (int i=0; i<matches.count(); i++) {
        matches[i]->setSelected(true);
    }

    // Resize the columns.
    directoriesTreeWidget->resizeColumnToContents(0);
}

void SeerSourceConfigPage::handleDownButtonClicked () {

    // Get the selected items.
    QList<QTreeWidgetItem*> matches = directoriesTreeWidget->selectedItems();

    // Loop through them from high to low.
    for (int i=matches.count()-1; i>=0; i--) {

        int index = directoriesTreeWidget->indexOfTopLevelItem(matches[i]);

        // Stop if the item is at the bottom of the list.
        if (index >= directoriesTreeWidget->topLevelItemCount()-1) {
            break;
        }

        // Steel it from the treeWidget.
        QTreeWidgetItem* topItem = directoriesTreeWidget->takeTopLevelItem(index);

        // Re-add it to the moved down position.
        directoriesTreeWidget->insertTopLevelItem(index+1, topItem);
    }

    // Re-selected the items.
    directoriesTreeWidget->clearSelection();
    for (int i=0; i<matches.count(); i++) {
        matches[i]->setSelected(true);
    }

    // Resize the columns.
    directoriesTreeWidget->resizeColumnToContents(0);
}

void SeerSourceConfigPage::handleDeleteButtonClicked () {

    QList<QTreeWidgetItem*> matches = directoriesTreeWidget->selectedItems();

    qDeleteAll(matches);
}

