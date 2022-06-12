#include "SeerEditorWidget.h"
#include <QtGui/QTextCursor>
#include <QtCore/QDebug>

SeerEditorWidgetMiniMapArea::SeerEditorWidgetMiniMapArea(SeerEditorWidgetSourceArea* editorWidget) : QWidget(editorWidget) {
    _editorWidget = editorWidget;
}

QSize SeerEditorWidgetMiniMapArea::sizeHint () const {
    return QSize(_editorWidget->miniMapAreaWidth(), 0);
}

void SeerEditorWidgetMiniMapArea::paintEvent (QPaintEvent* event) {
    _editorWidget->miniMapAreaPaintEvent(event);
}

void SeerEditorWidgetMiniMapArea::mouseDoubleClickEvent (QMouseEvent* event) {

    QTextCursor  cursor = _editorWidget->cursorForPosition(event->pos());

    qDebug() << cursor.blockNumber()+1;

    QWidget::mouseDoubleClickEvent(event);
}

void SeerEditorWidgetMiniMapArea::mouseMoveEvent (QMouseEvent* event) {

    QTextCursor  cursor = _editorWidget->cursorForPosition(event->pos());

    qDebug() << cursor.blockNumber()+1;

    QWidget::mouseMoveEvent(event);
}

void SeerEditorWidgetMiniMapArea::mousePressEvent (QMouseEvent* event) {

    QTextCursor  cursor = _editorWidget->cursorForPosition(event->pos());

    qDebug() << cursor.blockNumber()+1;

    QWidget::mousePressEvent(event);
}

void SeerEditorWidgetMiniMapArea::mouseReleaseEvent (QMouseEvent* event) {

    QTextCursor  cursor = _editorWidget->cursorForPosition(event->pos());

    qDebug() << cursor.blockNumber()+1;

    QWidget::mouseReleaseEvent(event);
}

//
//
//

SeerEditorWidgetAssemblyMiniMapArea::SeerEditorWidgetAssemblyMiniMapArea(SeerEditorWidgetAssemblyArea* editorWidget) : QWidget(editorWidget) {
    _editorWidget = editorWidget;
}

QSize SeerEditorWidgetAssemblyMiniMapArea::sizeHint () const {
    return QSize(_editorWidget->miniMapAreaWidth(), 0);
}

void SeerEditorWidgetAssemblyMiniMapArea::paintEvent (QPaintEvent* event) {
    _editorWidget->miniMapAreaPaintEvent(event);
}

void SeerEditorWidgetAssemblyMiniMapArea::mouseDoubleClickEvent (QMouseEvent* event) {

    QTextCursor  cursor = _editorWidget->cursorForPosition(event->pos());

    qDebug() << cursor.blockNumber()+1;

    QWidget::mouseDoubleClickEvent(event);
}

void SeerEditorWidgetAssemblyMiniMapArea::mouseMoveEvent (QMouseEvent* event) {

    QTextCursor  cursor = _editorWidget->cursorForPosition(event->pos());

    qDebug() << cursor.blockNumber()+1;

    QWidget::mouseMoveEvent(event);
}

void SeerEditorWidgetAssemblyMiniMapArea::mousePressEvent (QMouseEvent* event) {

    QTextCursor  cursor = _editorWidget->cursorForPosition(event->pos());

    qDebug() << cursor.blockNumber()+1;

    QWidget::mousePressEvent(event);
}

void SeerEditorWidgetAssemblyMiniMapArea::mouseReleaseEvent (QMouseEvent* event) {

    QTextCursor  cursor = _editorWidget->cursorForPosition(event->pos());

    qDebug() << cursor.blockNumber()+1;

    QWidget::mouseReleaseEvent(event);
}

