// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: MIT

#pragma once

#include <QtCharts/QChart>

#if QT_VERSION < 0x060000
using namespace QtCharts;
#endif

class QGestureEvent;

class QZoomChart : public QChart {

    public:
        QZoomChart(QGraphicsItem* parent = nullptr, Qt::WindowFlags wFlags = {});
       ~QZoomChart();

    protected:
        bool        sceneEvent      (QEvent* event);

    private:
        bool        gestureEvent    (QGestureEvent* event);

    private:

};

