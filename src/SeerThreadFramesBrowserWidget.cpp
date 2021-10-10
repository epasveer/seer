#include "SeerThreadFramesBrowserWidget.h"
#include "SeerUtl.h"
#include <QtWidgets/QTreeWidget>
#include <QtWidgets/QTreeWidgetItemIterator>
#include <QtWidgets/QApplication>
#include <QtCore/QDebug>

SeerThreadFramesBrowserWidget::SeerThreadFramesBrowserWidget (QWidget* parent) : QWidget(parent) {

    // Construct the UI.
    setupUi(this);

    // Setup the widgets
    threadTreeWidget->setMouseTracking(true);
    threadTreeWidget->setSortingEnabled(false);
    threadTreeWidget->resizeColumnToContents(0); // id
    threadTreeWidget->resizeColumnToContents(1); // target-id
    threadTreeWidget->resizeColumnToContents(2); // name
    threadTreeWidget->resizeColumnToContents(3); // level
    threadTreeWidget->resizeColumnToContents(4); // addr
    threadTreeWidget->resizeColumnToContents(5); // func
    threadTreeWidget->resizeColumnToContents(6); // args
    threadTreeWidget->resizeColumnToContents(7); // file
    threadTreeWidget->resizeColumnToContents(8); // fullname
    threadTreeWidget->resizeColumnToContents(9); // line
    threadTreeWidget->resizeColumnToContents(10); // arch
    threadTreeWidget->resizeColumnToContents(11); // state
    threadTreeWidget->resizeColumnToContents(12); // core

    threadTreeWidget->clear();

    // Connect things.
    QObject::connect(threadTreeWidget, &QTreeWidget::itemDoubleClicked,    this, &SeerThreadFramesBrowserWidget::handleItemDoubleClicked);
    QObject::connect(threadTreeWidget, &QTreeWidget::itemEntered,          this, &SeerThreadFramesBrowserWidget::handleItemEntered);
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

        //qDebug() << __PRETTY_FUNCTION__ << ":" << text;

        threadTreeWidget->clear();

        QString threads_text         = Seer::parseFirst(text, "threads=", '[', ']', false);
        QString currentthreadid_text = Seer::parseFirst(text, "current-thread-id=", '"', '"', false);

        //qDebug() << __PRETTY_FUNCTION__ << ":" << threads_text;

        if (threads_text != "") {

            QStringList threads_list = Seer::parse(threads_text, "", '{', '}', false);

            for ( const auto& thread_text : threads_list  ) {
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

                //qDebug() << __PRETTY_FUNCTION__ << ":" << file_text << fullname_text;

                // Add the frame to the tree.
                QTreeWidgetItem* item = new QTreeWidgetItem;
                item->setText(0, id_text);
                item->setText(1, targetid_text);
                item->setText(2, name_text);
                item->setText(3, level_text);
                item->setText(4, addr_text);
                item->setText(5, func_text);
                item->setText(6, args_text);
                item->setText(7, file_text);
                item->setText(8, fullname_text);
                item->setText(9, line_text);
                item->setText(10, arch_text);
                item->setText(11, state_text);
                item->setText(12, core_text);

                threadTreeWidget->addTopLevelItem(item);
            }

            // Select the current thread id.
            threadTreeWidget->clearSelection();

            QList<QTreeWidgetItem*> matches = threadTreeWidget->findItems(currentthreadid_text, Qt::MatchExactly, 0);
            if (matches.size() > 0) {
                threadTreeWidget->setCurrentItem(matches.first());
            }
        }

    }else if (text.startsWith("^error,msg=\"No registers.\"")) {
        threadTreeWidget->clear();

    }else{
        // Ignore others.
    }

    threadTreeWidget->resizeColumnToContents(0);
    threadTreeWidget->resizeColumnToContents(1);
    threadTreeWidget->resizeColumnToContents(2);
    threadTreeWidget->resizeColumnToContents(3);
    threadTreeWidget->resizeColumnToContents(4);
    threadTreeWidget->resizeColumnToContents(5);
  //threadTreeWidget->resizeColumnToContents(6);  // Don't resize.
    threadTreeWidget->resizeColumnToContents(7);
    threadTreeWidget->resizeColumnToContents(8);
    threadTreeWidget->resizeColumnToContents(9);
    threadTreeWidget->resizeColumnToContents(10);
    threadTreeWidget->resizeColumnToContents(11);
    threadTreeWidget->resizeColumnToContents(12);

    QApplication::restoreOverrideCursor();
}

void SeerThreadFramesBrowserWidget::handleStoppingPointReached () {

    // Don't do any work if the widget is hidden.
    if (isHidden()) {
        return;
    }

    refresh();
}

void SeerThreadFramesBrowserWidget::refresh () {
    emit refreshThreadFrames();
}

void SeerThreadFramesBrowserWidget::handleItemDoubleClicked (QTreeWidgetItem* item, int column) {

    Q_UNUSED(column);

    int lineno = item->text(9).toInt();

    //qDebug() << __PRETTY_FUNCTION__ << ":" << "Emit selectedFile and selectedFrame";

    emit selectedFile(item->text(7), item->text(8), lineno);

    // Comment out this signal. The frame number is always 0 from gdb. Why?
    //emit selectedFrame(item->text(3).toInt());
}

void SeerThreadFramesBrowserWidget::handleItemEntered (QTreeWidgetItem* item, int column) {

    //qDebug() << __PRETTY_FUNCTION__ << ":" << item->text(0) << column;

    item->setToolTip(0, item->text(0) + " : " + item->text(11) + " : " + item->text(5) + " : " + item->text(7) + " : " + item->text(9));

    for (int i=1; i<threadTreeWidget->columnCount(); i++) { // Copy tooltip to other columns.
        item->setToolTip(i, item->toolTip(0));
    }
}

void SeerThreadFramesBrowserWidget::showEvent (QShowEvent* event) {

    QWidget::showEvent(event);

    refresh();
}

