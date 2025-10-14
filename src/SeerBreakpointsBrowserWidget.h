// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include <QtWidgets/QWidget>
#include <QtCore/QString>
#include <QtCore/QStringList>
#include "ui_SeerBreakpointsBrowserWidget.h"

class SeerBreakpointsBrowserWidget : public QWidget, protected Ui::SeerBreakpointsBrowserWidgetForm {

    Q_OBJECT

    public:
        explicit SeerBreakpointsBrowserWidget (QWidget* parent = 0);
       ~SeerBreakpointsBrowserWidget ();

        bool                isEmpty                     () const;
        QStringList         breakpoints                 () const;

    public slots:
        void                handleText                  (const QString& text);
        void                handleStoppingPointReached  ();
        void                handleSessionTerminated     ();

    private slots:
        void                handleItemDoubleClicked     (QTreeWidgetItem* item, int column);
        void                handleRefreshToolButton     ();
        void                handleAddToolButton         ();
        void                handleDeleteToolButton      ();
        void                handleEnableToolButton      ();
        void                handleDisableToolButton     ();
        void                handleConditionToolButton   ();
        void                handleIgnoreToolButton      ();
        void                handleCommandsToolButton    ();

    signals:
        void                refreshBreakpointsList      ();
        void                deleteBreakpoints           (QString breakpoints);
        void                enableBreakpoints           (QString breakpoints);
        void                disableBreakpoints          (QString breakpoints);
        void                addBreakpointCondition      (QString breakpoint, QString condition);
        void                addBreakpointIgnore         (QString breakpoint, QString count);
        void                addBreakpointCommands       (QString breakpoint, QStringList commands);
        void                insertBreakpoint            (QString breakpoint);
        void                selectedFile                (QString file, QString fullname, int lineno);
        void                selectedAddress             (QString address);

    protected:
        void                showEvent                   (QShowEvent* event);

    private:
};

