#include "SeerEditorWidget.h"
#include <QtCore/QDebug>

SeerEditorWidgetLineNumberArea::SeerEditorWidgetLineNumberArea(SeerEditorWidgetSourceArea* editorWidget) : QWidget(editorWidget) {
    _editorWidget = editorWidget;
}

QSize SeerEditorWidgetLineNumberArea::sizeHint () const {
    return QSize(_editorWidget->lineNumberAreaWidth(), 0);
}

void SeerEditorWidgetLineNumberArea::paintEvent (QPaintEvent* event) {
    _editorWidget->lineNumberAreaPaintEvent(event);
}

void SeerEditorWidgetLineNumberArea::mouseDoubleClickEvent (QMouseEvent* event) {

    if (event->button() == Qt::LeftButton) {
        _editorWidget->setQuickBreakpoint(event);

    }else{
        QWidget::mouseDoubleClickEvent(event);
    }
}

void SeerEditorWidgetLineNumberArea::mouseMoveEvent (QMouseEvent* event) {

    QWidget::mouseMoveEvent(event);
}

void SeerEditorWidgetLineNumberArea::mousePressEvent (QMouseEvent* event) {

    if (event->button() == Qt::RightButton) {
        _editorWidget->showContextMenu(event);

    }else{
        QWidget::mousePressEvent(event);
    }

}

void SeerEditorWidgetLineNumberArea::mouseReleaseEvent (QMouseEvent* event) {

    QWidget::mouseReleaseEvent(event);
}

