#include "SeerEditorWidget.h"
#include <QtCore/QDebug>

SeerEditorWidgetSourceLineNumberArea::SeerEditorWidgetSourceLineNumberArea(SeerEditorWidgetSourceArea* editorWidget) : QWidget(editorWidget) {
    _editorWidget = editorWidget;
}

QSize SeerEditorWidgetSourceLineNumberArea::sizeHint () const {
    return QSize(_editorWidget->lineNumberAreaWidth(), 0);
}

void SeerEditorWidgetSourceLineNumberArea::paintEvent (QPaintEvent* event) {
    _editorWidget->lineNumberAreaPaintEvent(event);
}

void SeerEditorWidgetSourceLineNumberArea::mouseDoubleClickEvent (QMouseEvent* event) {

    if (event->button() == Qt::LeftButton) {
        _editorWidget->setQuickBreakpoint(event);

    }else{
        QWidget::mouseDoubleClickEvent(event);
    }
}

void SeerEditorWidgetSourceLineNumberArea::mouseMoveEvent (QMouseEvent* event) {

    QWidget::mouseMoveEvent(event);
}

void SeerEditorWidgetSourceLineNumberArea::mousePressEvent (QMouseEvent* event) {

    if (event->button() == Qt::RightButton) {
        _editorWidget->showContextMenu(event);

    }else{
        QWidget::mousePressEvent(event);
    }

}

void SeerEditorWidgetSourceLineNumberArea::mouseReleaseEvent (QMouseEvent* event) {

    QWidget::mouseReleaseEvent(event);
}

