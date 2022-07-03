#include "SeerEditorWidgetSource.h"
#include <QtCore/QDebug>

SeerEditorWidgetSourceBreakPointArea::SeerEditorWidgetSourceBreakPointArea(SeerEditorWidgetSourceArea* editorWidget) : QWidget(editorWidget) {
    _editorWidget = editorWidget;
}

QSize SeerEditorWidgetSourceBreakPointArea::sizeHint () const {
    return QSize(_editorWidget->breakPointAreaWidth(), 0);
}

void SeerEditorWidgetSourceBreakPointArea::paintEvent (QPaintEvent* event) {
    _editorWidget->breakPointAreaPaintEvent(event);
}

void SeerEditorWidgetSourceBreakPointArea::mouseDoubleClickEvent (QMouseEvent* event) {

    if (event->button() == Qt::LeftButton) {
        _editorWidget->setQuickBreakpoint(event);

    }else{
        QWidget::mouseDoubleClickEvent(event);
    }
}

void SeerEditorWidgetSourceBreakPointArea::mouseMoveEvent (QMouseEvent* event) {

    QWidget::mouseMoveEvent(event);
}

void SeerEditorWidgetSourceBreakPointArea::mousePressEvent (QMouseEvent* event) {

    if (event->button() == Qt::RightButton) {
        _editorWidget->showContextMenu(event);

    }else{
        QWidget::mousePressEvent(event);
    }
}

void SeerEditorWidgetSourceBreakPointArea::mouseReleaseEvent (QMouseEvent* event) {

    QWidget::mouseReleaseEvent(event);
}

