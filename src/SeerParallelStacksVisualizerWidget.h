// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include <QtWidgets/QWidget>
#include <QtCore/QVector>
#include "ui_SeerParallelStacksVisualizerWidget.h"

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
};



class SeerParallelStacksVisualizerWidget : public QWidget, protected Ui::SeerParallelStacksVisualizerWidgetForm {

    Q_OBJECT

    public:
        explicit SeerParallelStacksVisualizerWidget (QWidget* parent = 0);
       ~SeerParallelStacksVisualizerWidget ();

    signals:
        void                        refreshParallelStackFrames          (int id);

    public slots:
        void                        handleText                          (const QString& text);

    protected slots:
        void                        handleRefreshButton                 ();
        void                        handleHelpButton                    ();
        void                        handlePrintButton                   ();
        void                        handleSaveButton                    ();

    protected:
        void                        writeSettings                       ();
        void                        readSettings                        ();
        void                        resizeEvent                         (QResizeEvent* event);

    private:
        void                        createDirectedGraph                 ();

        int                         _id;
        Seer::PSV::ThreadsVector    _threads;
};

