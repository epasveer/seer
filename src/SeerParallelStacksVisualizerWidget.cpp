// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "SeerParallelStacksVisualizerWidget.h"
#include "SeerHelpPageDialog.h"
#include "SeerUtl.h"
#include <QtWidgets/QMessageBox>
#include <QtWidgets/QFileDialog>
#include <QtGui/QIntValidator>
#include <QtGui/QIcon>
#include <QtPrintSupport/QPrinter>
#include <QtPrintSupport/QPrintDialog>
#include <QtCore/QSettings>
#include <QtCore/QVector>
#include <QtCore/QDebug>

namespace Seer::PSV {

    class Frame {
        public:
            Frame ();
            Frame (const QString& text);
           ~Frame ();

            int                 level           () const;
            const QString&      addr            () const;
            const QString&      function        () const;
            const QString&      arch            () const;
            const QString&      file            () const;
            const QString&      fullname        () const;
            int                 line            () const;
            const QString&      type            () const;

        private:
            int                 _level;
            QString             _addr;
            QString             _function;
            QString             _arch;
            QString             _file;
            QString             _fullname;
            int                 _line;
            QString             _type;
    };

    typedef QVector<Frame> FramesVector;

    Frame::Frame() {
    }

    Frame::Frame(const QString& text) {

        _level     = Seer::parseFirst(text, "level=",    '"', '"', false).toInt();
        _addr      = Seer::parseFirst(text, "addr=",     '"', '"', false);
        _function  = Seer::parseFirst(text, "func=",     '"', '"', false);
        _arch      = Seer::parseFirst(text, "arch=",     '"', '"', false);
        _file      = Seer::parseFirst(text, "file=",     '"', '"', false);
        _fullname  = Seer::parseFirst(text, "fullname=", '"', '"', false);
        _line      = Seer::parseFirst(text, "line=",     '"', '"', false).toInt();
        _type      = Seer::parseFirst(text, "type=",     '"', '"', false);
    }

    Frame::~Frame() {
    }

    int Frame::level () const {
        return _level;
    }

    const QString& Frame::addr () const {
        return _addr;
    }

    const QString& Frame::function () const {
        return _function;
    }

    const QString& Frame::arch () const {
        return _arch;
    }

    const QString& Frame::file () const {
        return _file;
    }

    const QString& Frame::fullname () const {
        return _fullname;
    }

    int Frame::line () const {
        return _line;
    }

    const QString& Frame::type () const {
        return _type;
    }

    class Thread {
        public:
            Thread ();
            Thread (const QString& text);
           ~Thread ();

            int                 id              () const;
            const QString&      target_id       () const;
            const QString&      name            () const;
            const QString&      state           () const;
            int                 current         () const;

            int                 frameCount      () const;
            const Frame&        frame           (int i) const;

        private:
            int                 _id;
            QString             _target_id;
            QString             _name;
            QString             _state;
            int                 _current;

            FramesVector        _frames;
    };

    typedef QVector<Thread> ThreadsVector;

    Thread::Thread () {
    }

    Thread::Thread (const QString& text) {

        _id        = Seer::parseFirst(text, "thread-id=", '"', '"', false).toInt();
        _target_id = Seer::parseFirst(text, "target-id=", '"', '"', false);
        _name      = Seer::parseFirst(text, "name=",      '"', '"', false);
        _current   = Seer::parseFirst(text, "current=",   '"', '"', false).toInt();

        QString frames_text = Seer::parseFirst(text, "frames=", '[', ']', false);

        QStringList frame_list  = Seer::parse(frames_text, "", '{', '}', false);

        // Loop through each thread.
        for (const auto& frame_text : frame_list) {
            Frame frame(frame_text);
            _frames.push_back(frame);
        }
    }

    Thread::~Thread () {
    }

    int Thread::id () const {
        return _id;
    }

    const QString& Thread::target_id () const {
        return _target_id;
    }

    const QString& Thread::name () const {
        return _name;
    }

    const QString& Thread::state () const {
        return _state;
    }

    int Thread::current () const {
        return _current;
    }

    int Thread::frameCount () const {
        return _frames.size();
    }

    const Frame& Thread::frame (int i) const {
        return _frames[i];
    }
};


SeerParallelStacksVisualizerWidget::SeerParallelStacksVisualizerWidget (QWidget* parent) : QWidget(parent) {

    // Init variables.
    _id = Seer::createID(); // ID for parallelstacks command.

    // Set up UI.
    setupUi(this);

    // Setup the widgets
    setWindowIcon(QIcon(":/seer/resources/icons/hicolor/64x64/seergdb.png"));
    setWindowTitle("Seer ParallelStacks Visualizer");
    setAttribute(Qt::WA_DeleteOnClose);

    // Connect things.
    QObject::connect(refreshToolButton,  &QToolButton::clicked,      this,  &SeerParallelStacksVisualizerWidget::handleRefreshButton);
    QObject::connect(helpToolButton,     &QToolButton::clicked,      this,  &SeerParallelStacksVisualizerWidget::handleHelpButton);
    QObject::connect(printToolButton,    &QToolButton::clicked,      this,  &SeerParallelStacksVisualizerWidget::handlePrintButton);
    QObject::connect(saveToolButton,     &QToolButton::clicked,      this,  &SeerParallelStacksVisualizerWidget::handleSaveButton);

    // Restore window settings.
    readSettings();
}

SeerParallelStacksVisualizerWidget::~SeerParallelStacksVisualizerWidget () {
}

void SeerParallelStacksVisualizerWidget::handleText (const QString& text) {

    QApplication::setOverrideCursor(Qt::BusyCursor);

    if (text.contains(QRegularExpression("^([0-9]+)\\^done,frames="))) {

        // 4^done,frames=[
        //        {thread-id="10",target-id="(26131, 26146, 0)",name="hellothreads5",state="stopped",current="0",
        //            frames=[
        //                {level="0",addr="0x7ffff78a3c1e",func="__futex_abstimed_wait_common",arch="i386:x86-64",from="/lib64/libc.so.6",type="normal"},
        //                {level="1",addr="0x7ffff78a6548",func="pthread_cond_wait@@GLIBC_2.3.2",arch="i386:x86-64",from="/lib64/libc.so.6",type="normal"},
        //                {level="2",addr="0x404357",func="w2",arch="i386:x86-64",file="hellothreads5.cpp",fullname="/nas/erniep/Development/seer/tests/hellothreads5/hellothreads5.cpp",line="22",type="normal"},
        //              ]
        //        },
        //  ],
        //  current_thread_id="1",current_frame_level="0",total_threads="11",total_frames="128"

        QString id_text = text.section('^', 0,0);

        if (id_text.toInt() == _id) {

            Seer::PSV::ThreadsVector threads;

            QString result_text = Seer::parseFirst(text, "frames=", '[', ']', false);

            QStringList thread_list = Seer::parse(result_text, "", '{', '}', false);

            // Loop through each thread.
            for (const auto& thread_text : thread_list) {

                Seer::PSV::Thread thread(thread_text);

                threads.push_back(thread);

                //qDebug().noquote() << thread_text;
                //qDebug() << thread_id_text << target_id_text << name_text << current_text;
            }

            for (const auto& thread : threads ) {
                qDebug() << "Thread" << thread.id() << "has" << thread.frameCount() << "frames.";
            }
        }
    }

    QApplication::restoreOverrideCursor();
}

void SeerParallelStacksVisualizerWidget::handleRefreshButton () {

    // Clear the status.
    messageLineEdit->setText("");

    emit refreshParallelStackFrames(_id);
}

void SeerParallelStacksVisualizerWidget::handleHelpButton () {

    SeerHelpPageDialog* help = new SeerHelpPageDialog;
    help->loadFile(":/seer/resources/help/ParallelStacksVisualizer.md");
    help->show();
    help->raise();
}

void SeerParallelStacksVisualizerWidget::handlePrintButton () {

    imageViewer->print();
}

void SeerParallelStacksVisualizerWidget::handleSaveButton () {

    imageViewer->saveFileDialog("/tmp/temp.png");
}

void SeerParallelStacksVisualizerWidget::writeSettings() {

    QSettings settings;

    settings.beginGroup("parallelstacksvisualizerwindow");
    settings.setValue("size", size());
    settings.endGroup();
}

void SeerParallelStacksVisualizerWidget::readSettings() {

    QSettings settings;

    settings.beginGroup("parallelstacksvisualizerwindow");
    resize(settings.value("size", QSize(800, 400)).toSize());
    settings.endGroup();
}

void SeerParallelStacksVisualizerWidget::resizeEvent (QResizeEvent* event) {

    writeSettings();

    QWidget::resizeEvent(event);
}

