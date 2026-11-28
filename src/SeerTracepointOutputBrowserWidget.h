// SPDX-FileCopyrightText: 2026 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include <QtWidgets/QWidget>
#include <QtCore/QString>
#include <QtCore/QList>
#include <QtCore/QMap>
#include "ui_SeerTracepointOutputBrowserWidget.h"

//
// Shows the snapshots of the tracepoint selected in SeerTracepointsBrowserWidget.
//
class SeerTracepointOutputBrowserWidget : public QWidget, protected Ui::SeerTracepointOutputBrowserWidgetForm {

    Q_OBJECT

    public:
        explicit SeerTracepointOutputBrowserWidget (QWidget* parent = 0);
       ~SeerTracepointOutputBrowserWidget ();

    public slots:
        void                handleText                  (const QString& text);
        void                handleStoppingPointReached  ();
        void                handleSessionTerminated     ();
        void                handleTracepointSelected    (QString tracepoint);
        void                handleTraceFrameScanned     (int frameno, QString tracepoint);
        void                handleTraceScanFinished     ();

    private slots:
        void                handleTracepointClicked     ();
        void                handleSnapshotClicked       (QTreeWidgetItem* item, int column);
        void                handleHelpToolButton        ();
        void                handleSplitterMoved         (int pos, int index);

    signals:
        void                refreshTracepointsList      ();
        void                tracepointSelected          (QString tracepoint);
        void                refreshTraceStatus          ();
        void                refreshTraceCollected       ();
        void                findTraceFrame              (int frameno);                          // A negative number returns to the live program.
        void                scanTraceFrames             (QList<int> frames, int selectedFrame);

    protected:
        void                showEvent                   (QShowEvent* event);

    private:
        void                writeSettings               ();
        void                readSettings                ();
        void                handleTracepointTable       (const QString& text);
        void                handleTraceStatus           (const QString& text);
        void                handleTraceCollected        (const QString& text);
        void                setTraceFrame               (int frameno, const QString& tracepoint);
        void                refreshSnapshots            ();
        void                scanSnapshots               (bool all);
        void                updateViewing               ();
        void                updateButtons               ();
        bool                viewingTracepoint           () const;   // Is the selected snapshot from this tracepoint?
        QList<int>          snapshots                   () const;   // This tracepoint's known snapshots.

        QString             _tracepoint;        // Selected tracepoint. Blank if none.
        QString             _traceStartTime;    // Changes for each trace run.
        bool                _traceRunning;
        int                 _traceFrames;       // Number of snapshots, for all tracepoints.
        int                 _traceFrame;        // Selected snapshot. -1 for the live program.
        QMap<int,QString>   _owners;            // Snapshot to tracepoint.
        bool                _scanning;
        int                 _scannedFrames;     // Number of snapshots when last scanned. Prevents repeats.
};
