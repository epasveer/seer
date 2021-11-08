#include "SeerSlashProcWidget.h"
#include "QProcessInfo.h"
#include <QtWidgets/QTreeWidget>
#include <QtWidgets/QTreeWidgetItemIterator>
#include <QtWidgets/QApplication>
#include <QtGui/QFont>
#include <QtCore/QDebug>


//
// Custom tree item with numeric ordering on column 0.
//
class QProcessInfoWidgetItem : public QTreeWidgetItem {

    public:
        QProcessInfoWidgetItem(QTreeWidget* parent = 0) : QTreeWidgetItem(parent) {
        }

    private:
        bool operator< (const QTreeWidgetItem& other) const {

            int column = treeWidget()->sortColumn();

            if (column == 0) {
                return text(column).toLong() < other.text(column).toLong();
            }

            return text(column) < other.text(column);
        }
};


//
// The main widget starts here.
//
SeerSlashProcWidget::SeerSlashProcWidget (QWidget* parent) : QWidget(parent) {

    // Construct the UI.
    setupUi(this);

    // Setup the widgets
    searchLineEdit->setPlaceholderText("Search...");
    searchLineEdit->setClearButtonEnabled(true);
    processTreeWidget->setMouseTracking(true);
    processTreeWidget->resizeColumnToContents(0);
    processTreeWidget->resizeColumnToContents(1);
    processTreeWidget->resizeColumnToContents(2);
    processTreeWidget->setSortingEnabled(true);
    processTreeWidget->clear();

    // Connect things.
    QObject::connect(searchLineEdit,     &QLineEdit::textChanged,            this,  &SeerSlashProcWidget::handleSearchLineEdit);
    QObject::connect(refreshToolButton,  &QToolButton::clicked,              this,  &SeerSlashProcWidget::refresh);

    // Load the initial process list.
    refresh();
}

SeerSlashProcWidget::~SeerSlashProcWidget () {
}

int SeerSlashProcWidget::selectedPid () const {

    QList<QTreeWidgetItem*> items = processTreeWidget->selectedItems();

    if (items.size() == 0) {
        return -1;
    }

    return items[0]->text(0).toLong();
}

QString SeerSlashProcWidget::selectedName () const {

    QList<QTreeWidgetItem*> items = processTreeWidget->selectedItems();

    if (items.size() == 0) {
        return "";
    }

    return items[0]->text(1);
}

QString SeerSlashProcWidget::selectedCommandLine () const {

    QList<QTreeWidgetItem*> items = processTreeWidget->selectedItems();

    if (items.size() == 0) {
        return "";
    }

    return items[0]->text(2);
}

void SeerSlashProcWidget::refresh () {

    QApplication::setOverrideCursor(Qt::BusyCursor);

    // Scan the /proc file system.
    QProcessList list = QProcessInfo::enumerate();

    // Loop through each entry and add it to our view.
    processTreeWidget->clear();

    for (const QProcessInfo& info : list) {

        //qDebug() << info.pid() << " " << info.name() << " " << info.commandLine();

        QTreeWidgetItem* item = new QProcessInfoWidgetItem;
        item->setText(0, QString::number(info.pid()));
        item->setText(1, info.name());
        item->setText(2, info.commandLine());

        processTreeWidget->addTopLevelItem(item);
    }

    // Adjust the column widths.
    processTreeWidget->clearSelection();
    processTreeWidget->resizeColumnToContents(0);
    processTreeWidget->resizeColumnToContents(1);
    processTreeWidget->resizeColumnToContents(2);

    searchLineEdit->clear();

    QApplication::restoreOverrideCursor();
}

void SeerSlashProcWidget::handleSearchLineEdit (const QString& text) {


    // Set everything to a normal font. If there is no search text, unhide everything.
    // If there is search text, hide everything so the matching ones can be unhidden later on.
    QTreeWidgetItemIterator it(processTreeWidget);

    if (*it) {

        QFont f0 = (*it)->font(0);
        QFont f1 = (*it)->font(1);
        QFont f2 = (*it)->font(2);

        f0.setBold(false);
        f1.setBold(false);
        f2.setBold(false);

        if (text == "") {
            while (*it) {
                (*it)->setHidden(false); // No search text, unhide everything.
                (*it)->setFont(0,f0);
                (*it)->setFont(1,f1);
                (*it)->setFont(2,f2);
                ++it;
            }

        }else{
            while (*it) {
                (*it)->setHidden(true); // Has serach text, hide everything. Matching items to be unhidden below.
                (*it)->setFont(0,f0);
                (*it)->setFont(1,f1);
                (*it)->setFont(2,f2);
                ++it;
            }
        }
    }

    // Set selected items to a bold font and unhidden. Move to the first match.
    if (text != "") {

        QList<QTreeWidgetItem*> matches;

        if (text.contains('*')) {
            matches = processTreeWidget->findItems(text, Qt::MatchWildcard | Qt::MatchRecursive, 1);
        }else{
            matches = processTreeWidget->findItems(text, Qt::MatchStartsWith | Qt::MatchRecursive, 1);
        }

        QList<QTreeWidgetItem*>::const_iterator it = matches.begin();
        QList<QTreeWidgetItem*>::const_iterator e  = matches.end();

        if (it != e) {

            processTreeWidget->setCurrentItem(*it);

            QFont f0 = (*it)->font(0);
            QFont f1 = (*it)->font(1);
            QFont f2 = (*it)->font(2);

            f0.setBold(false);
            f1.setBold(true);
            f2.setBold(false);

            while (it != e) {
                (*it)->setHidden(false);
                (*it)->setFont(0,f0);
                (*it)->setFont(1,f1);
                (*it)->setFont(2,f2);

                it++;
            }
        }

        //qDebug() << __PRETTY_FUNCTION__ << ":" << text << matches.size();
    }

    processTreeWidget->resizeColumnToContents(0);
    processTreeWidget->resizeColumnToContents(1);
    processTreeWidget->resizeColumnToContents(2);
}

