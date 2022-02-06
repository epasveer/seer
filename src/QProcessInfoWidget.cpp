#include "QProcessInfoWidget.h"
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
QProcessInfoWidget::QProcessInfoWidget (QWidget* parent) : QWidget(parent) {

    // Construct the UI.
    setupUi(this);

    // Setup the widgets
    processTreeWidget->setMouseTracking(true);
    processTreeWidget->resizeColumnToContents(0);
    processTreeWidget->resizeColumnToContents(1);
    processTreeWidget->resizeColumnToContents(2);
    processTreeWidget->resizeColumnToContents(3);
    processTreeWidget->setSortingEnabled(true);
    processTreeWidget->clear();
    systemProcessesCheckBox->setChecked(false);

    // Connect things.
    QObject::connect(programNameLineEdit,     &QLineEdit::textChanged,            this,  &QProcessInfoWidget::refreshView);
    QObject::connect(userNameLineEdit,        &QLineEdit::textChanged,            this,  &QProcessInfoWidget::refreshView);
    QObject::connect(systemProcessesCheckBox, &QCheckBox::clicked,                this,  &QProcessInfoWidget::refreshView);
    QObject::connect(refreshToolButton,       &QToolButton::clicked,              this,  &QProcessInfoWidget::refreshList);
    QObject::connect(processTreeWidget,       &QTreeWidget::itemDoubleClicked,    this,  &QProcessInfoWidget::handleDoubleClicked);

    // Load the initial process list.
    refreshList();
}

QProcessInfoWidget::~QProcessInfoWidget () {
}

int QProcessInfoWidget::selectedPid () const {

    QList<QTreeWidgetItem*> items = processTreeWidget->selectedItems();

    if (items.size() == 0) {
        return -1;
    }

    return items[0]->text(0).toLong();
}

QString QProcessInfoWidget::selectedUsername () const {

    QList<QTreeWidgetItem*> items = processTreeWidget->selectedItems();

    if (items.size() == 0) {
        return "";
    }

    return items[0]->text(1);
}

QString QProcessInfoWidget::selectedName () const {

    QList<QTreeWidgetItem*> items = processTreeWidget->selectedItems();

    if (items.size() == 0) {
        return "";
    }

    return items[0]->text(2);
}

QString QProcessInfoWidget::selectedCommandLine () const {

    QList<QTreeWidgetItem*> items = processTreeWidget->selectedItems();

    if (items.size() == 0) {
        return "";
    }

    return items[0]->text(3);
}

void QProcessInfoWidget::refreshList () {

    QApplication::setOverrideCursor(Qt::BusyCursor);

    // Scan the /proc file system.
    QProcessList list = QProcessInfo::populate();

    // Loop through each entry and add it to our view.
    processTreeWidget->clear();

    for (const QProcessInfo& info : list) {

        QTreeWidgetItem* item = new QProcessInfoWidgetItem;
        item->setText(0, QString::number(info.pid()));
        item->setText(1, info.username());
        item->setText(2, info.name());
        item->setText(3, info.commandLine());

        processTreeWidget->addTopLevelItem(item);
    }

    // Adjust the column widths.
    processTreeWidget->clearSelection();
    processTreeWidget->resizeColumnToContents(0);
    processTreeWidget->resizeColumnToContents(1);
    processTreeWidget->resizeColumnToContents(2);
    processTreeWidget->resizeColumnToContents(3);

    // Don't clear the line edits.
    // programNameLineEdit->clear();
    // userNameLineEdit->clear();

    refreshView();

    QApplication::restoreOverrideCursor();
}

void QProcessInfoWidget::refreshView () {

    // Get this list of program name matches. Or all if there is no program name provided.
    QList<QTreeWidgetItem*> programNameMatches;

    if (programNameLineEdit->text() == "") {
        programNameMatches = processTreeWidget->findItems("*", Qt::MatchWildcard | Qt::MatchRecursive, 2);

    }else{
        if (programNameLineEdit->text().contains('*')) {
            programNameMatches = processTreeWidget->findItems(programNameLineEdit->text(), Qt::MatchWildcard   | Qt::MatchRecursive, 2);
        }else{
            programNameMatches = processTreeWidget->findItems(programNameLineEdit->text(), Qt::MatchStartsWith | Qt::MatchRecursive, 2);
        }
    }

    // Get this list of user name matches. Or all if there is no user name provided.
    QList<QTreeWidgetItem*> userNameMatches;

    if (userNameLineEdit->text() == "") {
        userNameMatches = processTreeWidget->findItems("*", Qt::MatchWildcard | Qt::MatchRecursive, 1);

    }else{
        if (userNameLineEdit->text().contains('*')) {
            userNameMatches = processTreeWidget->findItems(userNameLineEdit->text(), Qt::MatchWildcard   | Qt::MatchRecursive, 1);
        }else{
            userNameMatches = processTreeWidget->findItems(userNameLineEdit->text(), Qt::MatchStartsWith | Qt::MatchRecursive, 1);
        }
    }

    // Get this list of process matches. Include one's with [xxx]) or not.
    QList<QTreeWidgetItem*> processMatches;

    if (systemProcessesCheckBox->isChecked() == true) {
        processMatches = processTreeWidget->findItems("*", Qt::MatchWildcard | Qt::MatchRecursive, 2);
        //qDebug() << "Checkbox is on." << processMatches.size();

    }else{
        // To find [xxx] processes   :  "^(\\[).*(\\])$"
        // To exclude [xxx] processes:  "^(?!\\[).*(?!\\])$"
        processMatches = processTreeWidget->findItems("^(?!\\[).*(?!\\])$", Qt::MatchRegExp | Qt::MatchRecursive, 2);
        //qDebug() << "Checkbox is off." << processMatches.size();
    }

    // Go through each item in the tree. If it's in the user, program, and process matches, show it.
    // Otherwise, hide it.
    QTreeWidgetItemIterator it(processTreeWidget);

    while (*it) {

        if (programNameMatches.contains(*it) && userNameMatches.contains(*it) && processMatches.contains(*it)) {
            (*it)->setHidden(false);

        }else{
            (*it)->setHidden(true);
        }

        ++it;
    }

    // Resize the columns.
    processTreeWidget->resizeColumnToContents(0);
    processTreeWidget->resizeColumnToContents(1);
    processTreeWidget->resizeColumnToContents(2);
    processTreeWidget->resizeColumnToContents(3);
}

void QProcessInfoWidget::handleDoubleClicked () {

    emit pidSelected(selectedPid());
}

