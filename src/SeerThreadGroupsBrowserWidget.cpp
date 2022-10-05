#include "SeerThreadGroupsBrowserWidget.h"
#include "SeerUtl.h"
#include <QtWidgets/QTreeWidget>
#include <QtWidgets/QTreeWidgetItemIterator>
#include <QtWidgets/QApplication>
#include <QtCore/QFileInfo>
#include <QtCore/QDebug>

SeerThreadGroupsBrowserWidget::SeerThreadGroupsBrowserWidget (QWidget* parent) : QWidget(parent) {

    // Construct the UI.
    setupUi(this);

    // Setup the widgets
    groupTreeWidget->setMouseTracking(true);
    groupTreeWidget->setSortingEnabled(false);
    groupTreeWidget->resizeColumnToContents(0); // id
    groupTreeWidget->resizeColumnToContents(1); // type
    groupTreeWidget->resizeColumnToContents(2); // pid
    groupTreeWidget->resizeColumnToContents(3); // executable
    groupTreeWidget->resizeColumnToContents(4); // cores

    groupTreeWidget->clear();

    // Connect things.
    QObject::connect(groupTreeWidget,           &QTreeWidget::itemEntered,          this, &SeerThreadGroupsBrowserWidget::handleItemEntered);
    QObject::connect(gdbRunToolButton,          &QToolButton::clicked,              this, &SeerThreadGroupsBrowserWidget::handleGdbRunToolButton);
    QObject::connect(gdbStartToolButton,        &QToolButton::clicked,              this, &SeerThreadGroupsBrowserWidget::handleGdbStartToolButton);
    QObject::connect(gdbContinueToolButton,     &QToolButton::clicked,              this, &SeerThreadGroupsBrowserWidget::handleGdbContinueToolButton);
    QObject::connect(gdbInterruptToolButton,    &QToolButton::clicked,              this, &SeerThreadGroupsBrowserWidget::handleGdbInterruptToolButton);
}

SeerThreadGroupsBrowserWidget::~SeerThreadGroupsBrowserWidget () {
}

void SeerThreadGroupsBrowserWidget::handleText (const QString& text) {

    // Don't do any work if the widget is hidden.
    if (isHidden()) {
        return;
    }

    QApplication::setOverrideCursor(Qt::BusyCursor);

    if (text.startsWith("^done,groups=[")) {

        // ^done,groups=[
        //                     {
        //                         id="i1",
        //                         type="process",
        //                         pid="5424",
        //                         executable="/home/erniep/Development/Peak/src/seer/tests/helloinferior/helloinferior",
        //                         cores=["0"]
        //                     },
        //                     {   id="i2",
        //                         type="process",
        //                         pid="5432",
        //                         executable="/home/erniep/Development/Peak/src/seer/tests/helloinferior/helloinferior",
        //                         cores=["5"]
        //                     }
        //                  ]

        //qDebug() << text;

        groupTreeWidget->clear();

        QString groups_text = Seer::parseFirst(text, "groups=", '[', ']', false);

        //qDebug() << groups_text;

        if (groups_text != "") {

            QStringList groups_list = Seer::parse(groups_text, "", '{', '}', false);

            for ( const auto& group_text : groups_list ) {

                QString id_text    = Seer::parseFirst(group_text, "id=",         '"', '"', false);
                QString type_text  = Seer::parseFirst(group_text, "type=",       '"', '"', false);
                QString pid_text   = Seer::parseFirst(group_text, "pid=",        '"', '"', false);
                QString exe_text   = Seer::parseFirst(group_text, "executable=", '"', '"', false);
                QString cores_text = Seer::parseFirst(group_text, "cores=",      '[', ']', false);

                // Create the item.
                QTreeWidgetItem* item = new QTreeWidgetItem;
                item->setText(0, id_text);
                item->setText(1, type_text);
                item->setText(2, pid_text);
                item->setText(3, exe_text);
                item->setText(4, cores_text);

                // Add the frame to the tree.
                groupTreeWidget->addTopLevelItem(item);
            }
        }

    }else if (text.startsWith("=thread-created,id=")) {

        // =thread-created,id="2",group-id="i2"
        refresh();

    }else if (text.startsWith("=thread-exited,id=")) {

        // =thread-exited,id="2",group-id="i2"
        refresh();

    }else{
        // Ignore others.
    }

    groupTreeWidget->resizeColumnToContents(0);
    groupTreeWidget->resizeColumnToContents(1);
    groupTreeWidget->resizeColumnToContents(2);
  //groupTreeWidget->resizeColumnToContents(3); // Don't resize.
    groupTreeWidget->resizeColumnToContents(4);

    QApplication::restoreOverrideCursor();
}

void SeerThreadGroupsBrowserWidget::refresh () {

    emit refreshThreadIds();
    emit refreshThreadGroups();
}

void SeerThreadGroupsBrowserWidget::handleItemEntered (QTreeWidgetItem* item, int column) {

    Q_UNUSED(column);

    //qDebug() << item->text(0) << column;

    item->setToolTip(0, item->text(0) + " : " + item->text(1) + " : " + item->text(2) + " : " + item->text(3) + " : " + item->text(4));

    for (int i=1; i<groupTreeWidget->columnCount(); i++) { // Copy tooltip to other columns.
        item->setToolTip(i, item->toolTip(0));
    }
}

void SeerThreadGroupsBrowserWidget::handleGdbRunToolButton () {

    QList<QTreeWidgetItem*> items = groupTreeWidget->selectedItems();

    QList<QTreeWidgetItem*>::iterator i;

    for (i = items.begin(); i != items.end(); ++i) {

        QString groupid = (*i)->text(0);

        emit runThreadGroup(groupid);
    }
}

void SeerThreadGroupsBrowserWidget::handleGdbStartToolButton () {

    QList<QTreeWidgetItem*> items = groupTreeWidget->selectedItems();

    QList<QTreeWidgetItem*>::iterator i;

    for (i = items.begin(); i != items.end(); ++i) {

        QString groupid = (*i)->text(0);

        emit startThreadGroup(groupid);
    }
}

void SeerThreadGroupsBrowserWidget::handleGdbContinueToolButton () {

    QList<QTreeWidgetItem*> items = groupTreeWidget->selectedItems();

    QList<QTreeWidgetItem*>::iterator i;

    for (i = items.begin(); i != items.end(); ++i) {

        QString groupid = (*i)->text(0);

        emit continueThreadGroup(groupid);
    }
}

void SeerThreadGroupsBrowserWidget::handleGdbInterruptToolButton () {

    QList<QTreeWidgetItem*> items = groupTreeWidget->selectedItems();

    QList<QTreeWidgetItem*>::iterator i;

    for (i = items.begin(); i != items.end(); ++i) {

        QString groupid = (*i)->text(0);

        emit interruptThreadGroup(groupid);
    }
}

void SeerThreadGroupsBrowserWidget::showEvent (QShowEvent* event) {

    QWidget::showEvent(event);

    refresh();
}

