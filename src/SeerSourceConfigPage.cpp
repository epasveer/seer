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

    ignoreFilePatternsTreeWidget->clear();
    ignoreFilePatternsTreeWidget->setSortingEnabled(false);
    ignoreFilePatternsTreeWidget->setSelectionMode(QAbstractItemView::ExtendedSelection);
    ignoreFilePatternsTreeWidget->resizeColumnToContents(0); // file pattern

    miscFilePatternsTreeWidget->clear();
    miscFilePatternsTreeWidget->setSortingEnabled(false);
    miscFilePatternsTreeWidget->setSelectionMode(QAbstractItemView::ExtendedSelection);
    miscFilePatternsTreeWidget->resizeColumnToContents(0); // file pattern

    sourceFilePatternsTreeWidget->clear();
    sourceFilePatternsTreeWidget->setSortingEnabled(false);
    sourceFilePatternsTreeWidget->setSelectionMode(QAbstractItemView::ExtendedSelection);
    sourceFilePatternsTreeWidget->resizeColumnToContents(0); // file pattern

    headerFilePatternsTreeWidget->clear();
    headerFilePatternsTreeWidget->setSortingEnabled(false);
    headerFilePatternsTreeWidget->setSelectionMode(QAbstractItemView::ExtendedSelection);
    headerFilePatternsTreeWidget->resizeColumnToContents(0); // file pattern


    // Connect things.
    QObject::connect(addAlternateDirectoryToolButton,        &QToolButton::clicked,     this, &SeerSourceConfigPage::handleAddAlternateButtonClicked);
    QObject::connect(moveUpAlternateDirectoriesToolButton,   &QToolButton::clicked,     this, &SeerSourceConfigPage::handleUpAlternateButtonClicked);
    QObject::connect(moveDownAlternateDirectoriesToolButton, &QToolButton::clicked,     this, &SeerSourceConfigPage::handleDownAlternateButtonClicked);
    QObject::connect(deleteAlternateDirectoriesToolButton,   &QToolButton::clicked,     this, &SeerSourceConfigPage::handleDeleteAlternateButtonClicked);
    QObject::connect(addIgnoreFilePatternToolButton,         &QToolButton::clicked,     this, &SeerSourceConfigPage::handleAddIgnorePatternButtonClicked);
    QObject::connect(deleteIgnoreFilePatternesToolButton,    &QToolButton::clicked,     this, &SeerSourceConfigPage::handleDeleteIgnorePatternButtonClicked);
    QObject::connect(addMiscFilePatternToolButton,           &QToolButton::clicked,     this, &SeerSourceConfigPage::handleAddMiscPatternButtonClicked);
    QObject::connect(deleteMiscFilePatternsToolButton,       &QToolButton::clicked,     this, &SeerSourceConfigPage::handleDeleteMiscPatternButtonClicked);
    QObject::connect(addSourceFilePatternToolButton,         &QToolButton::clicked,     this, &SeerSourceConfigPage::handleAddSourcePatternButtonClicked);
    QObject::connect(deleteSourceFilePatternsToolButton,     &QToolButton::clicked,     this, &SeerSourceConfigPage::handleDeleteSourcePatternButtonClicked);
    QObject::connect(addHeaderFilePatternToolButton,         &QToolButton::clicked,     this, &SeerSourceConfigPage::handleAddHeaderPatternButtonClicked);
    QObject::connect(deleteHeaderFilePatternsToolButton,     &QToolButton::clicked,     this, &SeerSourceConfigPage::handleDeleteHeaderPatternButtonClicked);

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

void SeerSourceConfigPage::setIgnoreFilePatterns (const QStringList& filePatterns) {

    ignoreFilePatternsTreeWidget->clear();

    QStringListIterator iter(filePatterns);

    while (iter.hasNext()) {

        QTreeWidgetItem* topItem = new QTreeWidgetItem;
        topItem->setText(0, iter.next());

        ignoreFilePatternsTreeWidget->addTopLevelItem(topItem);
    }

    ignoreFilePatternsTreeWidget->resizeColumnToContents(0);
}

QStringList SeerSourceConfigPage::ignoreFilePatterns () const {

    QStringList list;

    QTreeWidgetItemIterator iter(ignoreFilePatternsTreeWidget);

    while (*iter) {
        list << (*iter)->text(0);
        ++iter;
    }

    return list;
}

void SeerSourceConfigPage::setMiscFilePatterns (const QStringList& filePatterns) {

    miscFilePatternsTreeWidget->clear();

    QStringListIterator iter(filePatterns);

    while (iter.hasNext()) {

        QTreeWidgetItem* topItem = new QTreeWidgetItem;
        topItem->setText(0, iter.next());

        miscFilePatternsTreeWidget->addTopLevelItem(topItem);
    }

    miscFilePatternsTreeWidget->resizeColumnToContents(0);
}

QStringList SeerSourceConfigPage::miscFilePatterns () const {

    QStringList list;

    QTreeWidgetItemIterator iter(miscFilePatternsTreeWidget);

    while (*iter) {
        list << (*iter)->text(0);
        ++iter;
    }

    return list;
}

void SeerSourceConfigPage::setSourceFilePatterns (const QStringList& filePatterns) {

    sourceFilePatternsTreeWidget->clear();

    QStringListIterator iter(filePatterns);

    while (iter.hasNext()) {

        QTreeWidgetItem* topItem = new QTreeWidgetItem;
        topItem->setText(0, iter.next());

        sourceFilePatternsTreeWidget->addTopLevelItem(topItem);
    }

    sourceFilePatternsTreeWidget->resizeColumnToContents(0);
}

QStringList SeerSourceConfigPage::sourceFilePatterns () const {

    QStringList list;

    QTreeWidgetItemIterator iter(sourceFilePatternsTreeWidget);

    while (*iter) {
        list << (*iter)->text(0);
        ++iter;
    }

    return list;
}

void SeerSourceConfigPage::setHeaderFilePatterns (const QStringList& filePatterns) {

    headerFilePatternsTreeWidget->clear();

    QStringListIterator iter(filePatterns);

    while (iter.hasNext()) {

        QTreeWidgetItem* topItem = new QTreeWidgetItem;
        topItem->setText(0, iter.next());

        headerFilePatternsTreeWidget->addTopLevelItem(topItem);
    }

    headerFilePatternsTreeWidget->resizeColumnToContents(0);
}

QStringList SeerSourceConfigPage::headerFilePatterns () const {

    QStringList list;

    QTreeWidgetItemIterator iter(headerFilePatternsTreeWidget);

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

    setIgnoreFilePatterns(QStringList());
    setMiscFilePatterns(   {"/usr/include/*"} );
    setSourceFilePatterns( {"*.cpp", "*.c", "*.C", "*.f", "*.f90", "*.F90", "*.rs", "*.go", "*.ada", "*.adb"} );
    setHeaderFilePatterns( {"*.hpp", "*.h", "*.ads"} );
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

    // Resize the columns and select the items we just deleted.
    alternateDirectoriesTreeWidget->resizeColumnToContents(0);
}

void SeerSourceConfigPage::handleAddIgnorePatternButtonClicked () {

    // Ask for the ignore directory to add to the list.
    QString directory = QInputDialog::getText(this, "Seer - Enter a directory to ignore", "Ignore directory:");

    if (directory == "") {
        return;
    }

    // Create the new item
    QTreeWidgetItem* topItem = new QTreeWidgetItem;
    topItem->setText(0, directory);

    // Just add it to the end of the list.
    ignoreFilePatternsTreeWidget->addTopLevelItem(topItem);

    // Resize the columns and select the item we just added.
    ignoreFilePatternsTreeWidget->resizeColumnToContents(0);
    ignoreFilePatternsTreeWidget->setCurrentItem(topItem, 0);
}

void SeerSourceConfigPage::handleDeleteIgnorePatternButtonClicked () {

    QList<QTreeWidgetItem*> matches = ignoreFilePatternsTreeWidget->selectedItems();

    qDeleteAll(matches);

    // Resize the columns and select the items we just deleted.
    ignoreFilePatternsTreeWidget->resizeColumnToContents(0);
}

void SeerSourceConfigPage::handleAddMiscPatternButtonClicked () {

    // Ask for the pattern to add to the list.
    QString pattern = QInputDialog::getText(this, "Seer - Enter a file pattern", "Misc file pattern:");

    if (pattern == "") {
        return;
    }

    // Create the new item
    QTreeWidgetItem* topItem = new QTreeWidgetItem;
    topItem->setText(0, pattern);

    // Just add it to the end of the list.
    miscFilePatternsTreeWidget->addTopLevelItem(topItem);

    // Resize the columns and select the item we just added.
    miscFilePatternsTreeWidget->resizeColumnToContents(0);
    miscFilePatternsTreeWidget->setCurrentItem(topItem, 0);
}

void SeerSourceConfigPage::handleDeleteMiscPatternButtonClicked () {

    QList<QTreeWidgetItem*> matches = miscFilePatternsTreeWidget->selectedItems();

    qDeleteAll(matches);

    // Resize the columns and select the items we just deleted.
    miscFilePatternsTreeWidget->resizeColumnToContents(0);
}

void SeerSourceConfigPage::handleAddSourcePatternButtonClicked () {

    // Ask for the pattern to add to the list.
    QString pattern = QInputDialog::getText(this, "Seer - Enter a file pattern", "Source file pattern:");

    if (pattern == "") {
        return;
    }

    // Create the new item
    QTreeWidgetItem* topItem = new QTreeWidgetItem;
    topItem->setText(0, pattern);

    // Just add it to the end of the list.
    sourceFilePatternsTreeWidget->addTopLevelItem(topItem);

    // Resize the columns and select the item we just added.
    sourceFilePatternsTreeWidget->resizeColumnToContents(0);
    sourceFilePatternsTreeWidget->setCurrentItem(topItem, 0);
}

void SeerSourceConfigPage::handleDeleteSourcePatternButtonClicked () {

    QList<QTreeWidgetItem*> matches = sourceFilePatternsTreeWidget->selectedItems();

    qDeleteAll(matches);

    // Resize the columns and select the items we just deleted.
    sourceFilePatternsTreeWidget->resizeColumnToContents(0);
}

void SeerSourceConfigPage::handleAddHeaderPatternButtonClicked () {

    // Ask for the pattern to add to the list.
    QString pattern = QInputDialog::getText(this, "Seer - Enter a file pattern", "Header file pattern:");

    if (pattern == "") {
        return;
    }

    // Create the new item
    QTreeWidgetItem* topItem = new QTreeWidgetItem;
    topItem->setText(0, pattern);

    // Just add it to the end of the list.
    headerFilePatternsTreeWidget->addTopLevelItem(topItem);

    // Resize the columns and select the item we just added.
    headerFilePatternsTreeWidget->resizeColumnToContents(0);
    headerFilePatternsTreeWidget->setCurrentItem(topItem, 0);
}

void SeerSourceConfigPage::handleDeleteHeaderPatternButtonClicked () {

    QList<QTreeWidgetItem*> matches = headerFilePatternsTreeWidget->selectedItems();

    qDeleteAll(matches);

    // Resize the columns and select the items we just deleted.
    headerFilePatternsTreeWidget->resizeColumnToContents(0);
}

