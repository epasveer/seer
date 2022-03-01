#include "SeerSourceBrowserWidget.h"
#include "SeerUtl.h"
#include <QtWidgets/QTreeWidget>
#include <QtWidgets/QTreeWidgetItemIterator>
#include <QtWidgets/QLabel>
#include <QtWidgets/QApplication>
#include <QtCore/QFileInfo>
#include <QtCore/Qt>
#include <QtCore/QDebug>

SeerSourceBrowserWidget::SeerSourceBrowserWidget (QWidget* parent) : QWidget(parent) {

    // Construct the UI.
    setupUi(this);

    // Setup the widgets
    sourceSearchLineEdit->setPlaceholderText("Search...");
    sourceSearchLineEdit->setClearButtonEnabled(true);
    sourceTreeWidget->setMouseTracking(true);
    sourceTreeWidget->resizeColumnToContents(0);
    sourceTreeWidget->resizeColumnToContents(1);
    sourceTreeWidget->clear();
    sourceTreeWidget->setSortingEnabled(false);

    _sourceFilesItems = 0;
    _headerFilesItems = 0;
    _miscFilesItems   = 0;

    // Connect things.
    QObject::connect(sourceTreeWidget,      &QTreeWidget::itemDoubleClicked,    this,  &SeerSourceBrowserWidget::handleItemDoubleClicked);
    QObject::connect(sourceTreeWidget,      &QTreeWidget::itemEntered,          this,  &SeerSourceBrowserWidget::handleItemEntered);
    QObject::connect(sourceSearchLineEdit,  &QLineEdit::textChanged,            this,  &SeerSourceBrowserWidget::handleSearchLineEdit);
}

SeerSourceBrowserWidget::~SeerSourceBrowserWidget () {
}

void SeerSourceBrowserWidget::handleText (const QString& text) {

    QApplication::setOverrideCursor(Qt::BusyCursor);

    if (text.startsWith("^done,files=[") && text.endsWith("]")) {

        sourceTreeWidget->clear();
        sourceTreeWidget->setSortingEnabled(false);
        sourceTreeWidget->sortByColumn(-1, Qt::AscendingOrder);

        _sourceFilesItems = new QTreeWidgetItem;
        _sourceFilesItems->setText(0, "Source files");

        _headerFilesItems = new QTreeWidgetItem;
        _headerFilesItems->setText(0, "Header files");

        _miscFilesItems = new QTreeWidgetItem;
        _miscFilesItems->setText(0, "Misc files");

        sourceTreeWidget->addTopLevelItem(_sourceFilesItems);
        sourceTreeWidget->addTopLevelItem(_headerFilesItems);
        sourceTreeWidget->addTopLevelItem(_miscFilesItems);

        // ^done,files=[
        //     {file=\"../sysdeps/x86_64/start.S\",fullname=\"/home/abuild/rpmbuild/BUILD/glibc-2.26/csu/../sysdeps/x86_64/start.S\"},
        //     {file=\"helloworld.cpp\",fullname=\"/home/erniep/Development/Peak/src/Seer/helloworld/helloworld.cpp\"}
        // ]

        QString files_text = Seer::parseFirst(text, "files=", '[', ']', false);

        //qDebug() << files_text;

        QStringList files_list = Seer::parse(files_text, "", '{', '}', false);

        for ( const auto& entry_text : files_list  ) {

            QString file_text     = Seer::parseFirst(entry_text, "file=",     '"', '"', false);
            QString fullname_text = Seer::parseFirst(entry_text, "fullname=", '"', '"', false);

            //qDebug() << file_text << fullname_text;

            // Get information about the file.
            QFileInfo fileInfo(fullname_text);

            // Add the file to the tree.
            QTreeWidgetItem* item = new QTreeWidgetItem;
            item->setText(0, QFileInfo(file_text).fileName());
            item->setText(1, fullname_text);

            /* Why have this?
            // Skip fully specified paths. Include files???
            if (file_text[0] == '/') {
                continue;
            }
            */

            // Look at the filename suffix.
            if (fileInfo.suffix() == "cpp" || fileInfo.suffix() == "c") { // C/C++
                _sourceFilesItems->addChild(item);

            }else if (fileInfo.suffix() == "f" || fileInfo.suffix() == "f90" || fileInfo.suffix() == "F90") { // Fortran
                _sourceFilesItems->addChild(item);

            }else if (fileInfo.suffix() == "rs") { // Rust
                _sourceFilesItems->addChild(item);

            }else if (fileInfo.suffix() == "hpp" || fileInfo.suffix() == "h") {
                _headerFilesItems->addChild(item);

            }else{
                _miscFilesItems->addChild(item);
            }
        }

        _sourceFilesItems->setExpanded(true);
        _headerFilesItems->setExpanded(false);
        _miscFilesItems->setExpanded(false);

    }else{
        // Ignore others.
    }

    sourceTreeWidget->resizeColumnToContents(0);
    sourceTreeWidget->resizeColumnToContents(1);
    sourceTreeWidget->sortByColumn(0, Qt::AscendingOrder);
    sourceTreeWidget->setSortingEnabled(true);

    sourceSearchLineEdit->clear();

    QApplication::restoreOverrideCursor();
}

void SeerSourceBrowserWidget::handleItemDoubleClicked (QTreeWidgetItem* item, int column) {

    Q_UNUSED(column);

    emit selectedFile(item->text(0), item->text(1), 0);
}

void SeerSourceBrowserWidget::handleItemEntered (QTreeWidgetItem* item, int column) {

    //qDebug() << item->text(0) << column;

    if (item->text(1) == "") { // Look at the FullName.
        for (int i=0; i<sourceTreeWidget->columnCount(); i++) { // The top-level items do not have a tooltip.
            item->setToolTip(i, "");
        }

    }else{
        item->setToolTip(0, item->text(0) + " : " + item->text(1));

        for (int i=1; i<sourceTreeWidget->columnCount(); i++) { // Copy tooltip to the other columns.
            item->setToolTip(i, item->toolTip(0));
        }
    }
}

void SeerSourceBrowserWidget::handleSearchLineEdit (const QString& text) {

    // Set everything to a normal font. If there is no search text, unhide everything.
    // If there is search text, hide everything so the matching ones can be unhidden later on.
    QTreeWidgetItemIterator it(sourceTreeWidget);

    if (*it) {

        QFont f0 = (*it)->font(0);
        QFont f1 = (*it)->font(1);

        f0.setBold(false);
        f1.setBold(false);

        if (text == "") {
            while (*it) {
                (*it)->setHidden(false); // No search text, unhide everything.
                (*it)->setFont(0,f0);
                (*it)->setFont(1,f1);
                ++it;
            }
        }else{
            while (*it) {
                (*it)->setHidden(true); // Has serach text, hide everything. Matching items to be unhidden below.
                (*it)->setFont(0,f0);
                (*it)->setFont(1,f1);
                ++it;
            }
        }
    }

    // Set selected items to a bold font and unhidden. Move to the first match.
    if (text != "") {

        _sourceFilesItems->setHidden(false);
        _headerFilesItems->setHidden(false);
        _miscFilesItems->setHidden(false);

        QList<QTreeWidgetItem*> matches;

        if (text.contains('*')) {
            matches = sourceTreeWidget->findItems(text, Qt::MatchWildcard | Qt::MatchRecursive, 0);
        }else{
            matches = sourceTreeWidget->findItems(text, Qt::MatchStartsWith | Qt::MatchRecursive, 0);
        }

        QList<QTreeWidgetItem*>::const_iterator it = matches.begin();
        QList<QTreeWidgetItem*>::const_iterator e  = matches.end();

        if (it != e) {

            sourceTreeWidget->setCurrentItem(*it);

            QFont f0 = (*it)->font(0);
            QFont f1 = (*it)->font(1);

            f0.setBold(true);
            f1.setBold(true);

            while (it != e) {
                if (*it != _sourceFilesItems && *it != _headerFilesItems && *it != _miscFilesItems) {
                    (*it)->setHidden(false);
                    (*it)->setFont(0,f0);
                    (*it)->setFont(1,f1);
                }
                it++;
            }
        }

        _sourceFilesItems->setExpanded(true);
        _headerFilesItems->setExpanded(false);
        _miscFilesItems->setExpanded(false);

        //qDebug() << text << matches.size();
    }

    sourceTreeWidget->resizeColumnToContents(0);
    sourceTreeWidget->resizeColumnToContents(1);
}

void SeerSourceBrowserWidget::refresh () {
    emit refreshSourceList();
}

