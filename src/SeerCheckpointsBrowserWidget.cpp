#include "SeerCheckpointsBrowserWidget.h"
#include "SeerUtl.h"
#include <QtWidgets/QTreeWidget>
#include <QtWidgets/QTreeWidgetItemIterator>
#include <QtWidgets/QApplication>
#include <QtWidgets/QMessageBox>
#include <QtCore/QDebug>

SeerCheckpointsBrowserWidget::SeerCheckpointsBrowserWidget (QWidget* parent) : QWidget(parent) {

    // Construct the UI.
    setupUi(this);

    // Setup the widgets
    checkpointsTreeWidget->clear();

    checkpointsTreeWidget->setSortingEnabled(false);
    checkpointsTreeWidget->setSelectionMode(QAbstractItemView::ExtendedSelection);
    checkpointsTreeWidget->resizeColumnToContents(0); // state
    checkpointsTreeWidget->resizeColumnToContents(1); // number
    checkpointsTreeWidget->resizeColumnToContents(2); // process
    checkpointsTreeWidget->resizeColumnToContents(3); // file
    checkpointsTreeWidget->resizeColumnToContents(4); // line

    // Connect things.
    QObject::connect(checkpointsTreeWidget,         &QTreeWidget::itemDoubleClicked,    this,  &SeerCheckpointsBrowserWidget::handleItemDoubleClicked);
    QObject::connect(refreshCheckpointsToolButton,  &QToolButton::clicked,              this,  &SeerCheckpointsBrowserWidget::handleRefreshToolButton);
    QObject::connect(addCheckpointToolButton,       &QToolButton::clicked,              this,  &SeerCheckpointsBrowserWidget::handleAddToolButton);
    QObject::connect(deleteCheckpointsToolButton,   &QToolButton::clicked,              this,  &SeerCheckpointsBrowserWidget::handleDeleteToolButton);
    QObject::connect(selectCheckpointToolButton,    &QToolButton::clicked,              this,  &SeerCheckpointsBrowserWidget::handleSelectToolButton);
}

SeerCheckpointsBrowserWidget::~SeerCheckpointsBrowserWidget () {
}

void SeerCheckpointsBrowserWidget::handleText (const QString& text) {

    // Don't do any work if the widget is hidden.
    if (isHidden()) {
        return;
    }

    if (text.startsWith("^done,checkpoints=[") && text.endsWith("]")) {

        //
        // "^done,checkpoints=[
        //                      {id="0",state="*",process="Thread 0x7ffff7e7f740 (LWP 31803) (main process) at 0x0",file="",line=""},
        //                      {id="1",state=" ",process="process 31806 at 0x55555555513f",file="hellostruct.cpp",line="49"},
        //                      {id="2",state=" ",process="process 31807 at 0x5555555552a0",file="hellostruct.cpp",line="62"}
        //                    ]
        //

        checkpointsTreeWidget->clear();
        checkpointsTreeWidget->setSortingEnabled(false);
        checkpointsTreeWidget->sortByColumn(-1, Qt::AscendingOrder);

        QString checkpoints_text = Seer::parseFirst(text, "checkpoints=", '[', ']', false);

        QStringList checkpoints_list = Seer::parse(checkpoints_text, "", '{', '}', false);

        for (const auto& checkpoint_entry : checkpoints_list) {

            QString id_text       = Seer::parseFirst(checkpoint_entry, "id=",      '"', '"', false);
            QString state_text    = Seer::parseFirst(checkpoint_entry, "state=",   '"', '"', false);
            QString process_text  = Seer::parseFirst(checkpoint_entry, "process=", '"', '"', false);
            QString file_text     = Seer::parseFirst(checkpoint_entry, "file=",    '"', '"', false);
            QString line_text     = Seer::parseFirst(checkpoint_entry, "line=",    '"', '"', false);

            // Add the function to the tree.
            QTreeWidgetItem* item = new QTreeWidgetItem;

            item->setText(0, state_text);
            item->setText(1, id_text);
            item->setText(2, process_text);
            item->setText(3, file_text);
            item->setText(4, line_text);

            checkpointsTreeWidget->addTopLevelItem(item);
        }
    }else{
        // Ignore others.
    }

    checkpointsTreeWidget->resizeColumnToContents(0);
    checkpointsTreeWidget->resizeColumnToContents(1);
    checkpointsTreeWidget->resizeColumnToContents(2);
    checkpointsTreeWidget->resizeColumnToContents(3);
    checkpointsTreeWidget->resizeColumnToContents(4);

    QApplication::restoreOverrideCursor();
}

void SeerCheckpointsBrowserWidget::handleStoppingPointReached () {

    // Don't do any work if the widget is hidden.
    if (isHidden()) {
        return;
    }

    emit refreshCheckpointsList();
}

void SeerCheckpointsBrowserWidget::handleSessionTerminated () {

    // Delete previous contents.
    checkpointsTreeWidget->clear();
}

void SeerCheckpointsBrowserWidget::handleItemDoubleClicked (QTreeWidgetItem* item, int column) {

    Q_UNUSED(column);

    emit selectCheckpoint(item->text(1));
}

void SeerCheckpointsBrowserWidget::handleRefreshToolButton () {

    emit refreshCheckpointsList();
}

void SeerCheckpointsBrowserWidget::handleAddToolButton () {

    // Otherwise send the command to create the checkpoint.
    emit insertCheckpoint();
}

void SeerCheckpointsBrowserWidget::handleSelectToolButton () {

    // Any items in the tree?
    if (checkpointsTreeWidget->topLevelItemCount() == 0) {
        QMessageBox::warning(this, "Seer", QString("There are no checkpoints to switch to."), QMessageBox::Ok, QMessageBox::Ok);
        return;
    }

    // Get selected tree items.
    QList<QTreeWidgetItem*> items = checkpointsTreeWidget->selectedItems();

    if (items.count() == 0) {
        QMessageBox::warning(this, "Seer", QString("Selected a checkpoint to switch to."), QMessageBox::Ok, QMessageBox::Ok);
        return;
    }

    if (items.count() > 1) {
        QMessageBox::warning(this, "Seer", QString("Select only 1 checkpoint to switch to."), QMessageBox::Ok, QMessageBox::Ok);
        return;
    }

    // Build a string that is a list of checkpoints.
    QString checkpoints;

    QList<QTreeWidgetItem*>::iterator i;
    for (i = items.begin(); i != items.end(); ++i) {

        if (i != items.begin()) {
            checkpoints += " ";
        }

        checkpoints += (*i)->text(1);
    }

    // Don't do anything if the list of checkpoints is empty.
    if (checkpoints == "") {
        return;
    }

    // Send the signal.
    emit selectCheckpoint(checkpoints);
}

void SeerCheckpointsBrowserWidget::handleDeleteToolButton () {

    // Any items in the tree?
    if (checkpointsTreeWidget->topLevelItemCount() == 0) {
        QMessageBox::warning(this, "Seer", QString("There are no checkpoints to delete."), QMessageBox::Ok, QMessageBox::Ok);
        return;
    }

    // Get selected tree items.
    QList<QTreeWidgetItem*> items =  checkpointsTreeWidget->selectedItems();

    if (items.count() == 0) {
        QMessageBox::warning(this, "Seer", QString("Select checkpoints to delete."), QMessageBox::Ok, QMessageBox::Ok);
        return;
    }

    // Build a string that is a list of checkpoints.
    QString checkpoints;

    QList<QTreeWidgetItem*>::iterator i;
    for (i = items.begin(); i != items.end(); ++i) {
        if (i != items.begin()) {
            checkpoints += " ";
        }
        checkpoints += (*i)->text(1);
    }

    // Don't do anything if the list of checkpoints is empty.
    if (checkpoints == "") {
        return;
    }

    // Send the signal.
    emit deleteCheckpoints(checkpoints);
}

void SeerCheckpointsBrowserWidget::showEvent (QShowEvent* event) {

    QWidget::showEvent(event);

    emit refreshCheckpointsList();
}

