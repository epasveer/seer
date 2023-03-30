#include "SeerAdaTasksBrowserWidget.h"
#include "SeerUtl.h"
#include <QtWidgets/QTreeWidget>
#include <QtWidgets/QTreeWidgetItemIterator>
#include <QtWidgets/QApplication>
#include <QtCore/QFileInfo>
#include <QtCore/QDebug>

SeerAdaTasksBrowserWidget::SeerAdaTasksBrowserWidget (QWidget* parent) : QWidget(parent) {

    // Construct the UI.
    setupUi(this);

    // Setup the widgets
    adaTaskTreeWidget->setMouseTracking(true);
    adaTaskTreeWidget->setSortingEnabled(false);
    adaTaskTreeWidget->resizeColumnToContents(0); // current
    adaTaskTreeWidget->resizeColumnToContents(1); // id
    adaTaskTreeWidget->resizeColumnToContents(2); // taskid
    adaTaskTreeWidget->resizeColumnToContents(3); // threadid
    adaTaskTreeWidget->resizeColumnToContents(4); // parentid
    adaTaskTreeWidget->resizeColumnToContents(5); // priority
    adaTaskTreeWidget->resizeColumnToContents(6); // state
    adaTaskTreeWidget->resizeColumnToContents(7); // name

    adaTaskTreeWidget->clear();

    // Connect things.
    QObject::connect(adaTaskTreeWidget,  &QTreeWidget::itemClicked,          this, &SeerAdaTasksBrowserWidget::handleItemClicked);
}

SeerAdaTasksBrowserWidget::~SeerAdaTasksBrowserWidget () {
}

void SeerAdaTasksBrowserWidget::handleText (const QString& text) {

    // Don't do any work if the widget is hidden.
    if (isHidden()) {
        return;
    }

    QApplication::setOverrideCursor(Qt::BusyCursor);

    if (text.startsWith("^done,tasks={")) {

        // -ada-task-info [ task-id ]

        // ^done,tasks={
        //                 nr_rows="3",
        //                 nr_cols="8",
        //                 hdr=[
        //                     {
        //                        width="1",alignment="-1",col_name="current",colhdr=""
        //                     },
        //                     {
        //                        width="3",alignment="1",col_name="id",colhdr="ID"
        //                     },
        //                     {
        //                        width="9",alignment="1",col_name="task-id",colhdr="TID"
        //                     },
        //                     {
        //                        width="4",alignment="1",col_name="thread-id",colhdr=""
        //                     },
        //                     {
        //                        width="4",alignment="1",col_name="parent-id",colhdr="P-ID"
        //                     },
        //                     {
        //                        width="3",alignment="1",col_name="priority",colhdr="Pri"
        //                     },
        //                     {
        //                        width="22",alignment="-1",col_name="state",colhdr="State"
        //                     },
        //                     {
        //                        width="1",alignment="2",col_name="name",colhdr="Name"
        //                     }
        //                    ],
        //                body=[
        //                      {
        //                          id="1",task-id="45d030",thread-id="1",priority="48",state="Waiting on RV with 2  ",name="main_task"
        //                      },
        //                      {
        //                          current="*",id="2",task-id="45de30",thread-id="2",parent-id="1",priority="48",state="Runnable",name="task_a"
        //                      },
        //                      {
        //                          id="3",task-id="461460",thread-id="3",parent-id="1",priority="48",state="Delay Sleep",name="task_b"
        //                      }
        //                     ]
        //            }


        //qDebug() << text;

        adaTaskTreeWidget->clear();

        QString body_text = Seer::parseFirst(text, "body=", '[', ']', false);

        //qDebug() << threads_text;

        if (body_text != "") {

            QStringList tasks_list = Seer::parse(body_text, "", '{', '}', false);

            for ( const auto& task_text : tasks_list ) {

                QString current_text  = Seer::parseFirst(task_text, "current=",   '"', '"', false);
                QString id_text       = Seer::parseFirst(task_text, "id=",        '"', '"', false);
                QString taskid_text   = Seer::parseFirst(task_text, "task-id=",   '"', '"', false);
                QString threadid_text = Seer::parseFirst(task_text, "thread-id=", '"', '"', false);
                QString parentid_text = Seer::parseFirst(task_text, "parent-id=", '"', '"', false);
                QString priority_text = Seer::parseFirst(task_text, "priority=",  '"', '"', false);
                QString state_text    = Seer::parseFirst(task_text, "state=",     '"', '"', false);
                QString name_text     = Seer::parseFirst(task_text, "name=",      '"', '"', false);

                // Create the item.
                QTreeWidgetItem* item = new QTreeWidgetItem;
                item->setText(0, current_text);
                item->setText(1, id_text);
                item->setText(2, taskid_text);
                item->setText(3, threadid_text);
                item->setText(4, parentid_text);
                item->setText(5, priority_text);
                item->setText(6, state_text);
                item->setText(7, name_text);

                // Add the frame to the tree.
                adaTaskTreeWidget->addTopLevelItem(item);
            }
        }

        // Clear the selection and select the one for the current thread-id.
        adaTaskTreeWidget->clearSelection();

        QList<QTreeWidgetItem*> matches = adaTaskTreeWidget->findItems("*", Qt::MatchExactly, 0);
        if (matches.size() > 0) {
            adaTaskTreeWidget->setCurrentItem(matches.first());
        }

    }else if (text.startsWith("^error,msg=\"No registers.\"")) {
        adaTaskTreeWidget->clear();

    }else{
        // Ignore others.
    }

    adaTaskTreeWidget->resizeColumnToContents(0);
    adaTaskTreeWidget->resizeColumnToContents(1);
    adaTaskTreeWidget->resizeColumnToContents(2);
    adaTaskTreeWidget->resizeColumnToContents(3);
    adaTaskTreeWidget->resizeColumnToContents(4);
    adaTaskTreeWidget->resizeColumnToContents(5);
    adaTaskTreeWidget->resizeColumnToContents(6);
    adaTaskTreeWidget->resizeColumnToContents(7);

    QApplication::restoreOverrideCursor();
}

void SeerAdaTasksBrowserWidget::handleStoppingPointReached () {

    // Don't do any work if the widget is hidden.
    if (isHidden()) {
        return;
    }

    refresh();
}

void SeerAdaTasksBrowserWidget::handleItemClicked (QTreeWidgetItem* item, int column) {

    Q_UNUSED(column);

    emit selectedThread(item->text(3).toInt());
}

void SeerAdaTasksBrowserWidget::refresh () {

    emit refreshAdaTasks();
}

void SeerAdaTasksBrowserWidget::showEvent (QShowEvent* event) {

    QWidget::showEvent(event);

    refresh();
}

