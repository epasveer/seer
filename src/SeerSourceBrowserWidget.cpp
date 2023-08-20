#include "SeerSourceBrowserWidget.h"
#include "SeerUtl.h"
#include <QtWidgets/QTreeWidget>
#include <QtWidgets/QTreeWidgetItemIterator>
#include <QtWidgets/QLabel>
#include <QtWidgets/QApplication>
#include <QtCore/QFileInfo>
#include <QtCore/Qt>
#include <QtCore/QMap>
#include <QtCore/QDebug>

SeerSourceBrowserWidget::SeerSourceBrowserWidget (QWidget* parent) : QWidget(parent) {

    // Construct the UI.
    setupUi(this);

    // Setup the widgets
    sourceSearchLineEdit->setPlaceholderText("Search regex...");
    sourceSearchLineEdit->setClearButtonEnabled(true);

    _sourceFilesItems = new QTreeWidgetItem;
    _sourceFilesItems->setText(0, "Source files");

    _headerFilesItems = new QTreeWidgetItem;
    _headerFilesItems->setText(0, "Header files");

    _miscFilesItems = new QTreeWidgetItem;
    _miscFilesItems->setText(0, "Misc files");

    sourceTreeWidget->setMouseTracking(true);
    sourceTreeWidget->addTopLevelItem(_sourceFilesItems);
    sourceTreeWidget->addTopLevelItem(_headerFilesItems);
    sourceTreeWidget->addTopLevelItem(_miscFilesItems);
    sourceTreeWidget->resizeColumnToContents(0);
    sourceTreeWidget->resizeColumnToContents(1);

    _sourceFilePatterns = QStringList( {"*.cpp", "*.c", "*.C", "*.f", "*.f90", ".F90", "*.rs", "*.go", "*.ada", "*.adb"} ); // Default settings.
    _headerFilePatterns = QStringList( {"*.hpp", "*.h", "*.ads"} );
    _miscFilePatterns   = QStringList( {"/usr/include/"} );

    // Connect things.
    QObject::connect(sourceTreeWidget,      &QTreeWidget::itemDoubleClicked,    this,  &SeerSourceBrowserWidget::handleItemDoubleClicked);
    QObject::connect(sourceTreeWidget,      &QTreeWidget::itemEntered,          this,  &SeerSourceBrowserWidget::handleItemEntered);
    QObject::connect(sourceSearchLineEdit,  &QLineEdit::textChanged,            this,  &SeerSourceBrowserWidget::handleSearchLineEdit);
}

SeerSourceBrowserWidget::~SeerSourceBrowserWidget () {
}

void SeerSourceBrowserWidget::setMiscFilePatterns (const QStringList& patterns) {

    _miscFilePatterns = patterns;
}

const QStringList& SeerSourceBrowserWidget::miscFilePatterns () const {

    return _miscFilePatterns;
}

void SeerSourceBrowserWidget::setSourceFilePatterns (const QStringList& patterns) {

    _sourceFilePatterns = patterns;
}

const QStringList& SeerSourceBrowserWidget::sourceFilePatterns () const {

    return _sourceFilePatterns;
}

void SeerSourceBrowserWidget::setHeaderFilePatterns (const QStringList& patterns) {

    _headerFilePatterns = patterns;
}

const QStringList& SeerSourceBrowserWidget::headerFilePatterns () const {

    return _headerFilePatterns;
}

void SeerSourceBrowserWidget::setIgnoreFilePatterns (const QStringList& patterns) {

    _ignoreFilePatterns = patterns;
}

const QStringList& SeerSourceBrowserWidget::ignoreFilePatterns () const {

    return _ignoreFilePatterns;
}

void SeerSourceBrowserWidget::handleText (const QString& text) {

    QApplication::setOverrideCursor(Qt::BusyCursor);

    if (text.startsWith("^done,files=[") && text.endsWith("]")) {

        // Delete previous files.
        foreach (auto i, _sourceFilesItems->takeChildren()) {
            delete i;
        }

        foreach (auto i, _headerFilesItems->takeChildren()) {
            delete i;
        }

        foreach (auto i, _miscFilesItems->takeChildren()) {
            delete i;
        }

        // ^done,files=[
        //     {file=\"../sysdeps/x86_64/start.S\",fullname=\"/home/abuild/rpmbuild/BUILD/glibc-2.26/csu/../sysdeps/x86_64/start.S\"},
        //     {file=\"helloworld.cpp\",fullname=\"/home/erniep/Development/Peak/src/Seer/helloworld/helloworld.cpp\"}
        // ]

        QString files_text     = Seer::parseFirst(text, "files=", '[', ']', false);
        QStringList files_list = Seer::parse(files_text, "", '{', '}', false);

        // Set up a map to look for duplicate entries.  QMap<fullname,file>
        QMap<QString,QString> files;

        for (const auto& entry_text : files_list) {

            QString file_text     = Seer::parseFirst(entry_text, "file=",     '"', '"', false);
            QString fullname_text = Seer::parseFirst(entry_text, "fullname=", '"', '"', false);

            //qDebug() << file_text << fullname_text;

            // Skip duplicates
            if (files.contains(fullname_text)) {
                continue;
            }

            files.insert(fullname_text, file_text);

            // Add the file to the tree.
            QTreeWidgetItem* item = new QTreeWidgetItem;
            item->setText(0, QFileInfo(file_text).fileName());
            item->setText(1, fullname_text);

            // See which pattern the file matches. Put the file under that folder.
            // If no match, put it in 'misc'.

            if (Seer::matchesWildcard(ignoreFilePatterns(), fullname_text)) {
                continue;
            }else if (Seer::matchesWildcard(miscFilePatterns(), fullname_text)) {
                _miscFilesItems->addChild(item);
            }else if (Seer::matchesWildcard(sourceFilePatterns(), fullname_text)) {
                _sourceFilesItems->addChild(item);
            }else if (Seer::matchesWildcard(headerFilePatterns(), fullname_text)) {
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

    Q_UNUSED(column);

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

        matches = sourceTreeWidget->findItems(text, Qt::MatchRegularExpression | Qt::MatchRecursive, 0);

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

