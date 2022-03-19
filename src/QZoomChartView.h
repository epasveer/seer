#pragma once

#include <QtCharts/QChartView>
#include <QtWidgets/QRubberBand>
#include <QtCore/QPointF>

using namespace QtCharts;

class QZoomChartView : public QChartView {

    public:
        QZoomChartView (QWidget* parent = 0);
        QZoomChartView (QtCharts::QChart* chart, QWidget* parent = 0);

    protected:
        bool                    viewportEvent               (QEvent*      event);
        void                    mousePressEvent             (QMouseEvent* event);
        void                    mouseMoveEvent              (QMouseEvent* event);
        void                    mouseReleaseEvent           (QMouseEvent* event);
        void                    wheelEvent                  (QWheelEvent* event);
        void                    keyPressEvent               (QKeyEvent*   event);
        void                    enterEvent                  (QEvent*      event);
        void                    leaveEvent                  (QEvent*      event);

    private:
        bool                    _isDragging;
        QPointF                 _lastMousePos;
        Qt::KeyboardModifiers   _keyboardModifiers;
};

