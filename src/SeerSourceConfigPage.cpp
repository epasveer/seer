#include "SeerSourceConfigPage.h"
#include <QtWidgets/QInputDialog>
#include <QtWidgets/QFileDialog>
#include <QtWidgets/QInputDialog>
#include <QtWidgets/QWidget>
#include <QtCore/QDebug>

SeerSourceConfigPage::SeerSourceConfigPage(QWidget* parent) : QWidget(parent) {

    // Set up the UI.
    setupUi(this);

    // Setup the widgets
    alternateDirectoriesTreeWidget->clear();
    alternateDirectoriesTreeWidget->setSortingEnabled(false);
    alternateDirectoriesTreeWidget->setSelectionMode(QAbstractItemView::ExtendedSelection);
    alternateDirectoriesTreeWidget->resizeColumnToContents(0); // directory
    ignoreDirectoriesTreeWidget->clear();
    ignoreDirectoriesTreeWidget->setSortingEnabled(false);
    ignoreDirectoriesTreeWidget->setSelectionMode(QAbstractItemView::ExtendedSelection);
    ignoreDirectoriesTreeWidget->resizeColumnToContents(0); // directory


    // Connect things.
    QObject::connect(addAlternateDirectoryToolButton,        &QToolButton::clicked,     this, &SeerSourceConfigPage::handleAddAlternateButtonClicked);
    QObject::connect(moveUpAlternateDirectoriesToolButton,   &QToolButton::clicked,     this, &SeerSourceConfigPage::handleUpAlternateButtonClicked);
    QObject::connect(moveDownAlternateDirectoriesToolButton, &QToolButton::clicked,     this, &SeerSourceConfigPage::handleDownAlternateButtonClicked);
    QObject::connect(deleteAlternateDirectoriesToolButton,   &QToolButton::clicked,     this, &SeerSourceConfigPage::handleDeleteAlternateButtonClicked);
    QObject::connect(addIgnoreDirectoryToolButton,           &QToolButton::clicked,     this, &SeerSourceConfigPage::handleAddIgnoreButtonClicked);
    QObject::connect(deleteIgnoreDirectoriesToolButton,      &QToolButton::clicked,     this, &SeerSourceConfigPage::handleDeleteIgnoreButtonClicked);

    // Setup the defaults.
    reset();
}

SeerSourceConfigPage::~SeerSourceConfigPage() {
}

void SeerSourceConfigPage::setAlternateDirectories (const QStringList& alternateDirectories) {

    alternateDirectoriesTreeWidget->clear();

    QStringListIterator iter(alternateDirectories);

    while (iter.hasNext()) {

        QTreeWidgetItem* topItem = new QTreeWidgetItem;
        topItem->setText(0, iter.next());

        alternateDirectoriesTreeWidget->addTopLevelItem(topItem);
    }

    alternateDirectoriesTreeWidget->resizeColumnToContents(0);
}

QStringList SeerSourceConfigPage::alternateDirectories () const {

    QStringList list;

    QTreeWidgetItemIterator iter(alternateDirectoriesTreeWidget);

    while (*iter) {
        list << (*iter)->text(0);
        ++iter;
    }

    return list;
}

void SeerSourceConfigPage::setIgnoreDirectories (const QStringList& ignoreDirectories) {

    ignoreDirectoriesTreeWidget->clear();

    QStringListIterator iter(ignoreDirectories);

    while (iter.hasNext()) {

        QTreeWidgetItem* topItem = new QTreeWidgetItem;
        topItem->setText(0, iter.next());

        ignoreDirectoriesTreeWidget->addTopLevelItem(topItem);
    }

    ignoreDirectoriesTreeWidget->resizeColumnToContents(0);
}

QStringList SeerSourceConfigPage::ignoreDirectories () const {

    QStringList list;

    QTreeWidgetItemIterator iter(ignoreDirectoriesTreeWidget);

    while (*iter) {
        list << (*iter)->text(0);
        ++iter;
    }

    return list;
}

void SeerSourceConfigPage::reset () {

    QStringList alternateDirectories;

    alternateDirectories << "./";

    setAlternateDirectories(alternateDirectories);
    setIgnoreDirectories(QStringList());

}

void SeerSourceConfigPage::handleAddAlternateButtonClicked () {

    // Ask for the alternate directory to add to the list.
    QString directory = QFileDialog::getExistingDirectory(this, "Seer - Enter an alternate directory", QString(), QFileDialog::ShowDirsOnly|QFileDialog::DontUseNativeDialog);

    if (directory == "") {
        return;
    }

    // Get the selected items, if any.
    QList<QTreeWidgetItem*> matches = alternateDirectoriesTreeWidget->selectedItems();

    // Create the new item
    QTreeWidgetItem* topItem = new QTreeWidgetItem;
    topItem->setText(0, directory);

    // If there is something selected, add the new item before the first selected item.
    if (matches.count() > 0) {

        int index = alternateDirectoriesTreeWidget->indexOfTopLevelItem(matches[0]);

        alternateDirectoriesTreeWidget->insertTopLevelItem(index, topItem);

    // Otherwise, just add it to the end of the list.
    }else{
        alternateDirectoriesTreeWidget->addTopLevelItem(topItem);
    }

    // Resize the columns and select the item we just added.
    alternateDirectoriesTreeWidget->resizeColumnToContents(0);

    alternateDirectoriesTreeWidget->setCurrentItem(topItem, 0);
}

void SeerSourceConfigPage::handleUpAlternateButtonClicked () {

    // Get the selected items.
    QList<QTreeWidgetItem*> matches = alternateDirectoriesTreeWidget->selectedItems();

    // Loop through them from low to high.
    for (int i=0; i<matches.count(); i++) {

        int index = alternateDirectoriesTreeWidget->indexOfTopLevelItem(matches[i]);

        // Stop if the item is at the top of the list.
        if (index <= 0) {
            break;
        }

        // Steel it from the treeWidget.
        QTreeWidgetItem* topItem = alternateDirectoriesTreeWidget->takeTopLevelItem(index);

        // Re-add it to the moved up position.
        alternateDirectoriesTreeWidget->insertTopLevelItem(index-1, topItem);
    }

    // Re-selected the items.
    alternateDirectoriesTreeWidget->clearSelection();
    for (int i=0; i<matches.count(); i++) {
        matches[i]->setSelected(true);
    }

    // Resize the columns.
    alternateDirectoriesTreeWidget->resizeColumnToContents(0);
}

void SeerSourceConfigPage::handleDownAlternateButtonClicked () {

    // Get the selected items.
    QList<QTreeWidgetItem*> matches = alternateDirectoriesTreeWidget->selectedItems();

    // Loop through them from high to low.
    for (int i=matches.count()-1; i>=0; i--) {

        int index = alternateDirectoriesTreeWidget->indexOfTopLevelItem(matches[i]);

        // Stop if the item is at the bottom of the list.
        if (index >= alternateDirectoriesTreeWidget->topLevelItemCount()-1) {
            break;
        }

        // Steel it from the treeWidget.
        QTreeWidgetItem* topItem = alternateDirectoriesTreeWidget->takeTopLevelItem(index);

        // Re-add it to the moved down position.
        alternateDirectoriesTreeWidget->insertTopLevelItem(index+1, topItem);
    }

    // Re-selected the items.
    alternateDirectoriesTreeWidget->clearSelection();
    for (int i=0; i<matches.count(); i++) {
        matches[i]->setSelected(true);
    }

    // Resize the columns.
    alternateDirectoriesTreeWidget->resizeColumnToContents(0);
}

void SeerSourceConfigPage::handleDeleteAlternateButtonClicked () {

    QList<QTreeWidgetItem*> matches = alternateDirectoriesTreeWidget->selectedItems();

    qDeleteAll(matches);
}

void SeerSourceConfigPage::handleAddIgnoreButtonClicked () {

    // Ask for the ignore directory to add to the list.
    QString directory = QInputDialog::getText(this, "Seer - Enter a directory to ignore", "Ignore directory:");

    if (directory == "") {
        return;
    }

    // Create the new item
    QTreeWidgetItem* topItem = new QTreeWidgetItem;
    topItem->setText(0, directory);

    // Just add it to the end of the list.
    ignoreDirectoriesTreeWidget->addTopLevelItem(topItem);

    // Resize the columns and select the item we just added.
    ignoreDirectoriesTreeWidget->resizeColumnToContents(0);

    ignoreDirectoriesTreeWidget->setCurrentItem(topItem, 0);
}

void SeerSourceConfigPage::handleDeleteIgnoreButtonClicked () {

    QList<QTreeWidgetItem*> matches = ignoreDirectoriesTreeWidget->selectedItems();

    qDeleteAll(matches);
}

