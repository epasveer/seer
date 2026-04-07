// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: MIT

#pragma once

#include <QtCharts>
#include <QtCharts/QChartView>
#include <QtWidgets/QRubberBand>
#include <QtCore/QPointF>

#if QT_VERSION < 0x060000
using namespace QtCharts;
#endif

class QZoomChartView : public QChartView {

    public:
        QZoomChartView (QWidget* parent = 0);
        QZoomChartView (QChart* chart, QWidget* parent = 0);

    public slots:
        void                    printView                   ();

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

