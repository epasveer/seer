#include "QZoomChartView.h"
#include <QtGui/QMouseEvent>
#include <QtGui/QGuiApplication>
#include <QtWidgets/QApplication>
#include <QtCore/QDebug>
#include <QtPrintSupport/QPrinter>
#include <QtPrintSupport/QPrintDialog>

QZoomChartView::QZoomChartView (QWidget* parent) : QChartView(parent) {

    _isDragging = false;

    setRubberBand(QChartView::RectangleRubberBand);
}

QZoomChartView::QZoomChartView (QChart* chart, QWidget* parent) : QChartView(chart, parent) {

    _isDragging = false;

    setRubberBand(QChartView::RectangleRubberBand);
}

void QZoomChartView::printView () {

    QPrinter printer;
    if (QPrintDialog(&printer).exec() == QDialog::Accepted) {
        QPainter painter(&printer);
        painter.setRenderHint(QPainter::Antialiasing);
        render(&painter);
    }
}

bool QZoomChartView::viewportEvent (QEvent* event) {

    // Disable ToolTip events in the ChartView. They interfer with the 'length' arguments
    // of the QToolTip::showText() function.
    // https://bugreports.qt.io/browse/QTBUG-56427

    switch (event->type()) {
        case QEvent::ToolTip:
            return false;
        default:
            break;
    }

    return QChartView::viewportEvent(event);
}

void QZoomChartView::mousePressEvent (QMouseEvent* event) {

    if (event->button() == Qt::LeftButton) {

        _keyboardModifiers = QGuiApplication::keyboardModifiers();

        if (_keyboardModifiers == Qt::ShiftModifier) {

            QApplication::setOverrideCursor(QCursor(Qt::SizeAllCursor));
            setRubberBand(QChartView::NoRubberBand);

            _lastMousePos = event->pos();
            _isDragging   = true;

            event->accept();

        }else{
            setRubberBand(QChartView::RectangleRubberBand);
        }
    }

    QChartView::mousePressEvent(event);
}

void QZoomChartView::mouseMoveEvent (QMouseEvent* event) {

    // pan the chart with a left mouse drag
    if (event->buttons() & Qt::LeftButton) {

        if (_keyboardModifiers == Qt::ShiftModifier) {

            auto dPos = event->pos() - _lastMousePos;

            chart()->scroll(-dPos.x(), dPos.y());
            chart()->update();

            _lastMousePos = event->pos();

            event->accept();
        }
    }

    QChartView::mouseMoveEvent(event);
}

void QZoomChartView::mouseReleaseEvent (QMouseEvent* event) {

    if (_isDragging) {
        QApplication::restoreOverrideCursor();
        _isDragging = false;
    }

    QChartView::mouseReleaseEvent(event);
}

void QZoomChartView::wheelEvent (QWheelEvent* event) {

    // https://stackoverflow.com/questions/48623595/scale-x-axis-of-qchartview-using-mouse-wheel

    qreal factor = event->angleDelta().y() > 0 ? 0.5 : 2.0;

    chart()->zoom(factor);
    chart()->update();

    event->accept();

    QChartView::wheelEvent(event);
}

void QZoomChartView::keyPressEvent (QKeyEvent* event) {

    switch (event->key()) {
        case Qt::Key_Plus:
            chart()->zoomIn();
            chart()->update();
            break;
        case Qt::Key_Minus:
            chart()->zoomOut();
            chart()->update();
            break;
        case Qt::Key_Escape:
            chart()->zoomReset();
            chart()->zoom(.9);
            chart()->update();
            break;
        case Qt::Key_Left:
            chart()->scroll(-10, 0);
            chart()->update();
            break;
        case Qt::Key_Right:
            chart()->scroll(10, 0);
            chart()->update();
            break;
        case Qt::Key_Up:
            chart()->scroll(0, 10);
            chart()->update();
            break;
        case Qt::Key_Down:
            chart()->scroll(0, -10);
            chart()->update();
            break;
        case Qt::Key_R:
            {
                if (event->modifiers() == Qt::ControlModifier) {
                    chart()->update();
                }
            }
            break;
        case Qt::Key_P:
            {
                if (event->modifiers() == Qt::ControlModifier) {
                    printView();
                }
            }
            break;
        default:
            QGraphicsView::keyPressEvent(event);
            break;
    }
}

void QZoomChartView::enterEvent (QEvent* event) {

    Q_UNUSED(event);

    setFocus();
}

void QZoomChartView::leaveEvent (QEvent* event) {
    Q_UNUSED(event);
}

