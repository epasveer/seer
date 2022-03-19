#pragma once

#include <QtCharts/QChart>

using namespace QtCharts;

class QGestureEvent;

class QZoomChart : public QChart {

    public:
        explicit QZoomChart(QGraphicsItem* parent = nullptr, Qt::WindowFlags wFlags = {});
       ~QZoomChart();

    protected:
        bool        sceneEvent      (QEvent* event);

    private:
        bool        gestureEvent    (QGestureEvent* event);

    private:

};

