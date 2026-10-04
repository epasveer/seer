// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include "SeerParallelStacksCommon.h"
#include <QtWidgets/QWidget>
#include <QString>
#include "ui_SeerParallelStacksVisualizerWidget.h"

class SeerParallelStacksVisualizerWidget : public QWidget, protected Ui::SeerParallelStacksVisualizerWidgetForm {

    Q_OBJECT

    public:
        explicit SeerParallelStacksVisualizerWidget (QWidget* parent = 0);
       ~SeerParallelStacksVisualizerWidget ();

        void                        setSettings                         (const SeerParallelStacksSettings& settings);
        SeerParallelStacksSettings  settings                            () const;

        void                        setShowFullFunctionName             (bool flag);
        bool                        showFullFunctionName                () const;
        void                        setFunctionNameLength               (int length);
        int                         functionNameLength                  () const;
        void                        setShowMinimapMode                  (const QString& mode);
        const QString&              showMinimapMode                     () const;

    signals:
        void                        refreshParallelStackFrames          (int id);
        void                        selectedThread                      (int threadId);

    public slots:
        void                        refresh                             ();
        void                        handleText                          (const QString& text);

        // Highlights threadId/frameLevel in the already-built graph — the
        // counterpart to createDirectedGraph(), which only builds and
        // deliberately leaves highlighting alone. Called internally right
        // after every createDirectedGraph() call (using _currentThreadId/
        // _currentFrameLevel), and exposed as a slot so another widget could
        // drive it directly if it ever has both values to give at once.
        void                        highlightDirectedGraph              (int threadId, int frameLevel);

        // Another widget (e.g. the thread frames browser) selected a
        // thread — reflect it in the graph without re-announcing it via
        // selectedThread(), since that widget already told gdb itself. Just
        // a thin wrapper around highlightDirectedGraph() using the
        // last-known frame level, since this signal only carries a thread id.
        void                        highlightSelectedThread             (int threadId);

        // Another widget (e.g. the stack frames browser) selected a frame —
        // reflect it in the graph the same way. The graph has no
        // frame-selection UI of its own, so there's no outgoing signal to
        // avoid re-announcing here. A thin wrapper around
        // highlightDirectedGraph() using the last-known thread id.
        void                        highlightSelectedFrame              (int frameLevel);

    protected slots:
        void                        handleRefreshButton                 ();
        void                        handleHelpButton                    ();
        void                        handlePrintButton                   ();
        void                        handleSaveButton                    ();
        void                        handleSettingsButton                ();
        void                        handleThemeChanged                  ();
        void                        handleMethodViewToggled             (bool checked);

        // A thread was picked in one of the graph's popup tables.
        void                        handleGraphThreadSelected           (int threadId);

    protected:
        void                        writeSettings                       ();
        void                        readSettings                        ();
        void                        resizeEvent                         (QResizeEvent* event);

    private:
        void                        createDirectedGraph                 ();

        bool                        isMethodView                        () const;

        // Method View's pivot: the function in the current thread's current
        // frame, or "" if that frame can't be found.
        QString                     currentPivotFunction                () const;

        // Selecting a different thread/frame re-pivots Method View (like
        // Visual Studio), so the graph has to be rebuilt, not just
        // re-highlighted. Threads view only needs the re-highlight.
        void                        updateForSelection                  ();

        int                         _id;
        SeerParallelStacksThreads   _threads;
        int                         _currentThreadId    = -1;
        int                         _currentFrameLevel  = 0;
        QString                     _methodPivot;           // the pivot the current Method View graph was built on
        SeerParallelStacksSettings  _settings;
};

