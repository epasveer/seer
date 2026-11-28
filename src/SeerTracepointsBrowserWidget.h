// SPDX-FileCopyrightText: 2026 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include <QtWidgets/QWidget>
#include <QtCore/QString>
#include <QtCore/QStringList>
#include <QtCore/QMap>
#include "ui_SeerTracepointsBrowserWidget.h"

class SeerTracepointsBrowserWidget : public QWidget, protected Ui::SeerTracepointsBrowserWidgetForm {

    Q_OBJECT

    public:
        explicit SeerTracepointsBrowserWidget (QWidget* parent = 0);
       ~SeerTracepointsBrowserWidget ();

        bool                isEmpty                     () const;

        static QString      traceStatusText             (const QMap<QString,QString>& map);     // From a -trace-status reply.

    public slots:
        void                handleText                  (const QString& text);
        void                handleStoppingPointReached  ();
        void                handleSessionTerminated     ();
        void                selectTracepoint            (QString tracepoint);

    private slots:
        void                handleSelectionChanged      ();
        void                handleItemDoubleClicked     (QTreeWidgetItem* item, int column);
        void                handleRefreshToolButton     ();
        void                handleAddToolButton         ();
        void                handleDeleteToolButton      ();
        void                handleEnableToolButton      ();
        void                handleDisableToolButton     ();
        void                handleConditionToolButton   ();
        void                handlePasscountToolButton   ();
        void                handleActionsToolButton     ();
        void                handleStartButton           ();
        void                handleSaveButton            ();
        void                handleHelpToolButton        ();

    signals:
        void                refreshTracepointsList      ();
        void                refreshTraceStatus          ();
        void                tracepointSelected          (QString tracepoint);       // Blank if none.
        void                showTracepointOutput        ();
        void                deleteTracepoints           (QString tracepoints);
        void                enableTracepoints           (QString tracepoints);
        void                disableTracepoints          (QString tracepoints);
        void                addBreakpointCondition      (QString tracepoint, QString condition);
        void                addBreakpointCommands       (QString tracepoint, QStringList commands);
        void                addTracepointPasscount      (QString tracepoint, int count);
        void                insertTracepoint            (QString parameters, QStringList actions, int passcount);
        void                startTrace                  ();
        void                stopTrace                   ();
        void                saveTrace                   (QString filename);
        void                selectedFile                (QString file, QString fullname, int lineno);

    protected:
        void                showEvent                   (QShowEvent* event);

    private:
        void                handleTracepointTable       (const QString& text);
        void                handleTraceStatus           (const QString& text);
        void                updateButtons               ();
        QString             currentTracepoint           () const;
        QString             selectedTracepoints         () const;
        QTreeWidgetItem*    selectedTracepoint          (const QString& what);

        QString             _traceSupported;    // "0", "1", or "file".
        bool                _traceRunning;
        int                 _traceFrames;       // Number of snapshots.
        QString             _tracepoint;        // Selected tracepoint. Blank if none.
};
