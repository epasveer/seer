// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once
#include "QProgressIndicator.h"

class SeerProgressIndicator : public QProgressIndicator {

    Q_OBJECT

    public:
        SeerProgressIndicator(QWidget* parent = 0);
       ~SeerProgressIndicator();

    protected:
        void                    writeSettings                       ();
        void                    readSettings                        ();

    protected slots:
        void                    handleShowContextMenu    (const QPoint& point);

    private:
};

