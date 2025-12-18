// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include <QtWidgets/QLabel>
#include <QtCore/QString>
#include <QGroupBox>
#include <QHBoxLayout>
#include <QLabel>

class SeerRunStatusIndicator : public QLabel {

    Q_OBJECT

    public:
        enum RunStatus {
            Idle    = 0,
            Stopped = 1,
            Stop_By_Breakpoint = 2,
            Running = 3,
        };

        explicit SeerRunStatusIndicator(QWidget* parent = 0);
       ~SeerRunStatusIndicator ();

        void                                setRunStatus                (SeerRunStatusIndicator::RunStatus status);
        SeerRunStatusIndicator::RunStatus   runStatus                   () const;

    signals:
        void                                statusChanged               (SeerRunStatusIndicator::RunStatus status);

    public slots:
        void                                handleText                  (const QString& text);
        void                                handleTerminate             ();

    protected:

    private:
        SeerRunStatusIndicator::RunStatus   _runStatus;
        QGroupBox                           *_groupBox;
        QLabel                              *_coreLabel;
        QLabel                              *_statusLabel;
        QHBoxLayout                         *_layout;
};

