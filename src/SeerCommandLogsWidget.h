// SPDX-FileCopyrightText: 2026 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include "SeerConsoleWidget.h"
#include "SeerGdbLogWidget.h"
#include "SeerSeerLogWidget.h"
#include "SeerMessagesBrowserWidget.h"
#include "SeerBreakpointsBrowserWidget.h"
#include "SeerWatchpointsBrowserWidget.h"
#include "SeerCatchpointsBrowserWidget.h"
#include "SeerPrintpointsBrowserWidget.h"
#include "SeerCheckpointsBrowserWidget.h"
#include <QtWidgets/QWidget>

#include "ui_SeerCommandLogsWidget.h"

class SeerCommandLogsWidget : public QWidget, protected Ui::SeerCommandLogsWidgetForm {

    Q_OBJECT

    public:
        explicit SeerCommandLogsWidget (QWidget* parent = 0);
        ~SeerCommandLogsWidget ();

        SeerMessagesBrowserWidget*          messagesBrowser                 ();
        SeerBreakpointsBrowserWidget*       breakpointsBrowser              ();
        SeerWatchpointsBrowserWidget*       watchpointsBrowser              ();
        SeerCatchpointsBrowserWidget*       catchpointsBrowser              ();
        SeerPrintpointsBrowserWidget*       printpointsBrowser              ();
        SeerCheckpointsBrowserWidget*       checkpointsBrowser              ();
        SeerGdbLogWidget*                   gdbOutputLog                    ();
        SeerSeerLogWidget*                  seerOutputLog                   ();

        void                                createConsole                   ();
        void                                deleteConsole                   ();
        void                                reattachConsole                 ();
        SeerConsoleWidget*                  console                         ();
        void                                setConsoleMode                  (const QString& mode);
        QString                             consoleMode                     () const;
        void                                setConsoleScrollLines           (int count);
        int                                 consoleScrollLines              () const;

        void                                setManualCommands               (const QStringList& commands);
        QStringList                         manualCommands                  (int count) const;
        void                                setRememberManualCommandCount   (int count);
        int                                 rememberManualCommandCount      () const;
        void                                clearManualCommandHistory       ();

    protected slots:
        void                                handleLogsTabMoved              (int to, int from);
        void                                handleLogsTabChanged            (int index);
        void                                handleRaiseMessageTab           ();
        void                                handleConsoleModeChanged        ();
        void                                handleConsoleNewTextAdded       ();
        void                                handleConsoleNewTextViewed      ();
        void                                handleManualCommandExecute      ();
        void                                handleManualCommandChanged      ();
        void                                handleLogOutputChanged          ();
        void                                handleGdbLoadBreakpoints        ();
        void                                handleGdbSaveBreakpoints        ();
        void                                handleHelpToolButtonClicked     ();

    signals:
        void                                executeGdbCommand               (QString command);
        void                                refreshBreakpointsList          ();

    protected:
        void                                writeSettings                   ();
        void                                readSettings                    ();

    private:
        SeerConsoleWidget*                  _consoleWidget;
        int                                 _consoleIndex;
        QString                             _consoleMode;
        int                                 _consoleScrollLines;
        int                                 _rememberManualCommandCount;

        SeerMessagesBrowserWidget*          _messagesBrowserWidget;
        SeerBreakpointsBrowserWidget*       _breakpointsBrowserWidget;
        SeerWatchpointsBrowserWidget*       _watchpointsBrowserWidget;
        SeerCatchpointsBrowserWidget*       _catchpointsBrowserWidget;
        SeerPrintpointsBrowserWidget*       _printpointsBrowserWidget;
        SeerCheckpointsBrowserWidget*       _checkpointsBrowserWidget;
        SeerGdbLogWidget*                   _gdbOutputLog;
        SeerSeerLogWidget*                  _seerOutputLog;
};

