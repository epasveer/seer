// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "SeerThreadFramesBrowserWidget.h"
#include "SeerUtl.h"
#include <QtWidgets/QTreeWidget>
#include <QtWidgets/QTreeWidgetItemIterator>
#include <QtWidgets/QApplication>
#include <QtCore/QFileInfo>
#include <QtCore/QDebug>

SeerThreadFramesBrowserWidget::SeerThreadFramesBrowserWidget (QWidget* parent) : QWidget(parent) {

    // Construct the UI.
    setupUi(this);

    // Setup the widgets
    threadTreeWidget->setMouseTracking(true);
    threadTreeWidget->setSortingEnabled(false);
    threadTreeWidget->resizeColumnToContents(0); // active
    threadTreeWidget->resizeColumnToContents(1); // id
    threadTreeWidget->resizeColumnToContents(2); // state
    threadTreeWidget->resizeColumnToContents(3); // target-id
    threadTreeWidget->resizeColumnToContents(4); // func
    threadTreeWidget->resizeColumnToContents(5); // file
    threadTreeWidget->resizeColumnToContents(6); // line
    threadTreeWidget->resizeColumnToContents(7); // fullname
    threadTreeWidget->resizeColumnToContents(8); // args
    threadTreeWidget->resizeColumnToContents(9); // name
    threadTreeWidget->resizeColumnToContents(10); // level
    threadTreeWidget->resizeColumnToContents(11); // addr
    threadTreeWidget->resizeColumnToContents(12); // arch
    threadTreeWidget->resizeColumnToContents(13); // core

    threadTreeWidget->clear();

    // Connect things.
    QObject::connect(threadTreeWidget,          &QTreeWidget::itemClicked,          this, &SeerThreadFramesBrowserWidget::handleItemClicked);
    QObject::connect(threadTreeWidget,          &QTreeWidget::itemEntered,          this, &SeerThreadFramesBrowserWidget::handleItemEntered);
    QObject::connect(gdbNextToolButton,         &QToolButton::clicked,              this, &SeerThreadFramesBrowserWidget::handleGdbNextToolButton);
    QObject::connect(gdbStepToolButton,         &QToolButton::clicked,              this, &SeerThreadFramesBrowserWidget::handleGdbStepToolButton);
    QObject::connect(gdbFinishToolButton,       &QToolButton::clicked,              this, &SeerThreadFramesBrowserWidget::handleGdbFinishToolButton);
    QObject::connect(gdbContinueToolButton,     &QToolButton::clicked,              this, &SeerThreadFramesBrowserWidget::handleGdbContinueToolButton);
    QObject::connect(gdbInterruptToolButton,    &QToolButton::clicked,              this, &SeerThreadFramesBrowserWidget::handleGdbInterruptToolButton);
}

SeerThreadFramesBrowserWidget::~SeerThreadFramesBrowserWidget () {
}

void SeerThreadFramesBrowserWidget::handleText (const QString& text) {

    // Don't do any work if the widget is hidden.
    if (isHidden()) {
        return;
    }

    QApplication::setOverrideCursor(Qt::BusyCursor);

    if (text.startsWith("^done,threads=[")) {

        //
        // ^done,threads=[
        //     {
        //          id=\"1\",
        //          target-id=\"Thread 0x7ffff7fbd740 (LWP 22356)\",
        //          name=\"hellothreads\",
        //          thread={
        //              level=\"0\",
        //              addr=\"0x00000000004006bf\",
        //              func=\"main\",
        //              args=[],
        //              file=\"hellothreads.cpp\",
        //              fullname=\"/home/erniep/Development/Peak/src/Seer/hellothreads/hellothreads.cpp\",
        //              line=\"27\",
        //              arch=\"i386:x86-64\"
        //          },
        //          state=\"stopped\",
        //          core=\"0\"
        //     },
        //
        //     {
        //          id=\"2\",
        //          target-id=\"Thread 0x7ffff6ed1700 (LWP 22370)\",
        //          details=\"Exiting\",
        //          name=\"hellothreads\",
        //          frame={
        //              level=\"0\",
        //              addr=\"0x00007ffff6f0b1c3\",
        //              func=\"__longjmp\",
        //              args=[],
        //              from=\"/lib64/libc.so.6\",
        //              arch=\"i386:x86-64\"
        //          },
        //          state=\"stopped\",
        //          core=\"6\"
        //     }
        // ],
        // current-thread-id=\"1\"
        //

        //qDebug() << text;

        threadTreeWidget->clear();

        QString threads_text         = Seer::parseFirst(text, "threads=", '[', ']', false);
        QString currentthreadid_text = Seer::parseFirst(text, "current-thread-id=", '"', '"', false);

        //qDebug() << threads_text;

        if (threads_text != "") {

            QStringList threads_list = Seer::parse(threads_text, "", '{', '}', false);

            for ( const auto& thread_text : threads_list ) {

                QString id_text       = Seer::parseFirst(thread_text, "id=",        '"', '"', false);
                QString targetid_text = Seer::parseFirst(thread_text, "target-id=", '"', '"', false);
                QString name_text     = Seer::parseFirst(thread_text, "name=",      '"', '"', false);

                QString frame_text    = Seer::parseFirst(thread_text, "frame=",     '{', '}', false);
                QString level_text    = Seer::parseFirst(frame_text,  "level=",     '"', '"', false);
                QString addr_text     = Seer::parseFirst(frame_text,  "addr=",      '"', '"', false);
                QString func_text     = Seer::parseFirst(frame_text,  "func=",      '"', '"', false);
                QString args_text     = Seer::parseFirst(frame_text,  "args=",      '[', ']', false);
                QString file_text     = Seer::parseFirst(frame_text,  "file=",      '"', '"', false);
                QString fullname_text = Seer::parseFirst(frame_text,  "fullname=",  '"', '"', false);
                QString line_text     = Seer::parseFirst(frame_text,  "line=",      '"', '"', false);
                QString arch_text     = Seer::parseFirst(frame_text,  "arch=",      '"', '"', false);

                QString state_text    = Seer::parseFirst(thread_text, "state=",     '"', '"', false);
                QString core_text     = Seer::parseFirst(thread_text, "core=",      '"', '"', false);

                //qDebug() << file_text << fullname_text;

                // Create the item.
                QTreeWidgetItem* item = new QTreeWidgetItem;

                if (id_text == currentthreadid_text) {
                    item->setText(0, "*");
                }else{
                    item->setText(0, " ");
                }

                item->setText(1, id_text);
                item->setText(2, state_text);
                item->setText(3, targetid_text);
                item->setText(4, func_text);
                item->setText(5, QFileInfo(file_text).fileName());
                item->setText(6, line_text);
                item->setText(7, fullname_text);
                item->setText(8, args_text);
                item->setText(9, name_text);
                item->setText(10, level_text);
                item->setText(11, addr_text);
                item->setText(12, arch_text);
                item->setText(13, core_text);

                // Add the frame to the tree.
                threadTreeWidget->addTopLevelItem(item);
            }

            // Select the current thread id.
            threadTreeWidget->clearSelection();

            QList<QTreeWidgetItem*> matches = threadTreeWidget->findItems(currentthreadid_text, Qt::MatchExactly, 1);
            if (matches.size() > 0) {
                threadTreeWidget->setCurrentItem(matches.first());
            }
        }

    }else if (text.startsWith("*running,thread-id=")) {

        refresh();

    }else if (text.startsWith("=thread-created,id=")) {

        // =thread-created,id="2",group-id="i2"
        refresh();

    }else if (text.startsWith("=thread-exited,id=")) {

        // =thread-exited,id="2",group-id="i2"
        refresh();

    }else{
        // Ignore others.
    }

    threadTreeWidget->resizeColumnToContents(0);
    threadTreeWidget->resizeColumnToContents(1);
    threadTreeWidget->resizeColumnToContents(2);
    threadTreeWidget->resizeColumnToContents(3);
  //threadTreeWidget->resizeColumnToContents(4); // Don't resize.
  //threadTreeWidget->resizeColumnToContents(5); // Don't resize.
    threadTreeWidget->resizeColumnToContents(6);
  //threadTreeWidget->resizeColumnToContents(7); // Don't resize.
  //threadTreeWidget->resizeColumnToContents(8); // Don't resize.
    threadTreeWidget->resizeColumnToContents(9);
    threadTreeWidget->resizeColumnToContents(10);
    threadTreeWidget->resizeColumnToContents(11);
    threadTreeWidget->resizeColumnToContents(12);
    threadTreeWidget->resizeColumnToContents(13);

    QApplication::restoreOverrideCursor();
}

void SeerThreadFramesBrowserWidget::handleStoppingPointReached () {

    refresh();
}

void SeerThreadFramesBrowserWidget::handleSessionTerminated () {

    // Delete previous contents.
    threadTreeWidget->clear();
}

void SeerThreadFramesBrowserWidget::refresh () {

    // Don't do any work if the widget is hidden.
    if (isHidden()) {
        return;
    }

    emit refreshThreadIds();
    emit refreshThreadFrames();
}

void SeerThreadFramesBrowserWidget::handleItemClicked (QTreeWidgetItem* item, int column) {

    Q_UNUSED(column);

    QList<QTreeWidgetItem*> items = threadTreeWidget->selectedItems();

    if (items.count() == 1) {

        int lineno = item->text(6).toInt();

        //qDebug() << "Emit selectedFile and selectedFrame";

        emit selectedFile(item->text(5), item->text(7), lineno);
        emit selectedThread(item->text(1).toInt());
    }
}

void SeerThreadFramesBrowserWidget::handleItemEntered (QTreeWidgetItem* item, int column) {

    Q_UNUSED(column);

    //qDebug() << item->text(1) << column;

    item->setToolTip(0, item->text(1) + " : " + item->text(2) + " : " + item->text(3) + " : " + item->text(5) + " : " + item->text(6));

    for (int i=1; i<threadTreeWidget->columnCount(); i++) { // Copy tooltip to other columns.
        item->setToolTip(i, item->toolTip(0));
    }
}

void SeerThreadFramesBrowserWidget::handleGdbNextToolButton () {

    QList<QTreeWidgetItem*> items = threadTreeWidget->selectedItems();

    QList<QTreeWidgetItem*>::iterator i;

    for (i = items.begin(); i != items.end(); ++i) {

        int threadid = (*i)->text(1).toInt();

        emit nextThreadId(threadid);
    }
}

void SeerThreadFramesBrowserWidget::handleGdbStepToolButton () {

    QList<QTreeWidgetItem*> items = threadTreeWidget->selectedItems();

    QList<QTreeWidgetItem*>::iterator i;

    for (i = items.begin(); i != items.end(); ++i) {

        int threadid = (*i)->text(1).toInt();

        emit stepThreadId(threadid);
    }
}

void SeerThreadFramesBrowserWidget::handleGdbFinishToolButton () {

    QList<QTreeWidgetItem*> items = threadTreeWidget->selectedItems();

    QList<QTreeWidgetItem*>::iterator i;

    for (i = items.begin(); i != items.end(); ++i) {

        int threadid = (*i)->text(1).toInt();

        emit finishThreadId(threadid);
    }
}

void SeerThreadFramesBrowserWidget::handleGdbContinueToolButton () {

    QList<QTreeWidgetItem*> items = threadTreeWidget->selectedItems();

    QList<QTreeWidgetItem*>::iterator i;

    for (i = items.begin(); i != items.end(); ++i) {

        int threadid = (*i)->text(1).toInt();

        emit continueThreadId(threadid);
    }
}

void SeerThreadFramesBrowserWidget::handleGdbInterruptToolButton () {

    QList<QTreeWidgetItem*> items = threadTreeWidget->selectedItems();

    QList<QTreeWidgetItem*>::iterator i;

    for (i = items.begin(); i != items.end(); ++i) {

        int threadid = (*i)->text(1).toInt();

        emit interruptThreadId(threadid);
    }
}

void SeerThreadFramesBrowserWidget::showEvent (QShowEvent* event) {

    QWidget::showEvent(event);

    refresh();
}

