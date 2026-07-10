#include "SeerParallelStacksGraphicsView.h"
#include <QPainter>
#include <QPainterPath>
#include <QWheelEvent>
#include <QScrollBar>
#include <QFontMetrics>
#include <QGraphicsSceneMouseEvent>
#include <QCursor>
#include <QHeaderView>
#include <QVBoxLayout>
#include <algorithm>
#include <cmath>

class SeerParallelStacksPopupTableWidget;

// ================================================================
// SeerParallelStacksStackBoxItem
// ================================================================
SeerParallelStacksStackBoxItem::SeerParallelStacksStackBoxItem(const SeerParallelStacksStack& stack, QGraphicsItem* parent) : QGraphicsItem(parent) {

    // Enable geometry-change notifications so itemChange() fires on setPos()
    setFlag(QGraphicsItem::ItemSendsGeometryChanges, true);
    setAcceptHoverEvents(true);

    _headerLeft = QString("%1 Thread%2") .arg(stack.threadCount) .arg(stack.threadCount == 1 ? "" : "s");

    _threadIds.resize(0);
    _rows.resize(0);

    if (!stack.threadIds.isEmpty()) {

        _threadIds = stack.threadIds;

        int shown = std::min<qsizetype>(_threadIds.size(), 8);

        QStringList parts;
        parts.reserve(shown);

        for (int i = 0; i < shown; ++i) {
            parts.append(QString::number(_threadIds[i]));
        }

        if (_threadIds.size() > 8) {
            _headerRight = QString("[%1 … +%2]").arg(parts.join(", ")).arg(_threadIds.size() - 8);
        }else{
            _headerRight = "[" + parts.join(", ") + "]";
        }
    }

    for (int i = stack.functions.size() - 1; i >= 0; --i) {
        _rows.append({ stack.functions[i], Function });
    }

    QFont        boldFont;  boldFont.setBold(true);
    QFontMetrics boldFm(boldFont);
    QFont        normFont;
    QFontMetrics normFm(normFont);

    qreal headerW  = boldFm.horizontalAdvance(_headerLeft) + boldFm.horizontalAdvance(_headerRight) + kHeaderGap;
    qreal maxTextW = headerW;

    for (const auto& row : _rows) {
        maxTextW = std::max(maxTextW, (qreal)normFm.horizontalAdvance(row.text));
    }

    _width  = maxTextW + 2 * kPadX;
    _height = kPadY + kRowH * (1 + (int)_rows.size()) + kPadY;
}

SeerParallelStacksStackBoxItem::~SeerParallelStacksStackBoxItem() {

    // Tell every edge that still points at us to forget about it.
    // This must run BEFORE this object's memory is freed, regardless of
    // whether the scene destroys this box or its edges first — it prevents
    // the edges' own destructors (and any pending itemChange callbacks)
    // from dereferencing a dangling pointer back to this item.

    for (SeerParallelStacksLiveEdge* e : _edges) {
        e->detachEndpoint(this);
    }

    _edges.clear();
}

QRectF SeerParallelStacksStackBoxItem::boundingRect() const {

    return QRectF(0, 0, _width, _height);
}

void SeerParallelStacksStackBoxItem::paint(QPainter* painter, const QStyleOptionGraphicsItem*, QWidget*) {

    painter->setRenderHint(QPainter::Antialiasing);

    painter->setBrush(QColor(0xFA, 0xFA, 0xFA));
    painter->setPen(QPen(QColor(0x88, 0x88, 0x88), 1.5));
    painter->drawRoundedRect(boundingRect(), 6, 6);

    QFont boldFont;  boldFont.setBold(true);
    QFont normFont;
    const qreal innerW = _width - 2 * kPadX;
    qreal y = kPadY;

    painter->setFont(boldFont);
    painter->setPen(QColor(0x22, 0x22, 0x22));
    painter->drawText(QRectF(kPadX, y, innerW, kRowH), Qt::AlignLeft | Qt::AlignVCenter, _headerLeft);

    if (!_headerRight.isEmpty()) {
        painter->setPen(QColor(0x1A, 0x52, 0xA8));
        painter->drawText(QRectF(kPadX, y, innerW, kRowH), Qt::AlignRight | Qt::AlignVCenter, _headerRight);
    }

    y += kRowH;

    painter->setPen(QPen(QColor(0xCC, 0xCC, 0xCC), 1));
    painter->drawLine(QPointF(0, y), QPointF(_width, y));

    painter->setFont(normFont);
    painter->setPen(QColor(0x00, 0x7A, 0x33));

    for (const auto& row : _rows) {
        painter->drawText(QRectF(kPadX, y, innerW, kRowH), Qt::AlignLeft | Qt::AlignVCenter, row.text);
        y += kRowH;
    }

    if (_dragging) {
        painter->setBrush(Qt::NoBrush);
        painter->setPen(QPen(QColor(0x1A, 0x52, 0xA8), 2.0, Qt::DashLine));
        painter->drawRoundedRect(boundingRect().adjusted(1, 1, -1, -1), 6, 6);
    }
}

QPointF SeerParallelStacksStackBoxItem::sceneBottom() const {

    return mapToScene(QPointF(_width / 2.0, _height));
}

QPointF SeerParallelStacksStackBoxItem::sceneTop() const {

    return mapToScene(QPointF(_width / 2.0, 0));
}

QVariant SeerParallelStacksStackBoxItem::itemChange(GraphicsItemChange change, const QVariant& value) {

    if (change == ItemPositionHasChanged) {
        for (SeerParallelStacksLiveEdge* e : _edges) {
            e->update();   // ask each connected edge to repaint
        }
    }

    return QGraphicsItem::itemChange(change, value);
}

void SeerParallelStacksStackBoxItem::mousePressEvent(QGraphicsSceneMouseEvent* event) {

    if (event->button() == Qt::LeftButton && event->modifiers() & Qt::ShiftModifier) {

        _dragging   = true;
        _dragOffset = event->pos();

        setZValue(10);
        update();

        event->accept();

        return;
    }

    QGraphicsItem::mousePressEvent(event);
}

void SeerParallelStacksStackBoxItem::mouseMoveEvent(QGraphicsSceneMouseEvent* event) {

    if (_dragging) {

        setPos(mapToScene(event->pos() - _dragOffset));
        event->accept();

        return;
    }

    QGraphicsItem::mouseMoveEvent(event);
}

void SeerParallelStacksStackBoxItem::mouseReleaseEvent(QGraphicsSceneMouseEvent* event) {

    if (_dragging && event->button() == Qt::LeftButton) {

        _dragging = false;

        setZValue(0);
        update();

        event->accept();

        return;
    }

    QGraphicsItem::mouseReleaseEvent(event);
}

void SeerParallelStacksStackBoxItem::hoverEnterEvent(QGraphicsSceneHoverEvent* event) {

    QGraphicsItem::hoverEnterEvent(event);

    if (_popup == nullptr) {

        QString function;

        if (_rows.size() > 0) {
            function = _rows[0].text;
        }

        _popup = new SeerParallelStacksPopupTableWidget();

        for (const auto& id : _threadIds) {
            _popup->addRow(id, function);
        }

        QGraphicsView* view = scene()->views().first(); // scene() is always valid here

        // Position below the item, aligned to its left edge
        QRectF sceneRect  = mapToScene(boundingRect()).boundingRect();
        QPoint viewportPt = view->mapFromScene(sceneRect.bottomLeft());
        QPoint globalPt   = view->viewport()->mapToGlobal(viewportPt);
        _popup->move(globalPt);

        _popup->show();

        // Connect things.
        QObject::connect(_popup, &SeerParallelStacksPopupTableWidget::mouseLeftPopup,      this, &SeerParallelStacksStackBoxItem::handleDeletePopup);
    }
}

void SeerParallelStacksStackBoxItem::hoverLeaveEvent(QGraphicsSceneHoverEvent* event) {

    QGraphicsItem::hoverLeaveEvent(event);
}

void SeerParallelStacksStackBoxItem::handleDeletePopup() {

    if (_popup) {
        delete _popup;
        _popup = nullptr;
    }
}

// ================================================================
// SeerParallelStacksLiveEdge — redraws itself each paint() from current endpoint positions
// ================================================================
SeerParallelStacksLiveEdge::SeerParallelStacksLiveEdge(SeerParallelStacksStackBoxItem* from, SeerParallelStacksStackBoxItem* to, QGraphicsItem* parent) : QGraphicsItem(parent) , _from(from) , _to(to) {

    setZValue(-1);

    // Position the edge item at the scene origin; all coordinates are scene-space.
    setPos(0, 0);

    // Edges are purely decorative and must never intercept mouse input.
    // Without this, QGraphicsView::itemAt() / scene hit-testing uses the
    // default shape() (== boundingRect(), which includes generous padding
    // for the bezier + arrowhead) and would report a LiveEdge under the
    // cursor for most clicks near/between boxes — silently swallowing
    // both box-grab and background-pan attempts before they ever reach
    // StackBoxItem or the view's own mousePressEvent.
    setAcceptedMouseButtons(Qt::NoButton);

    _from->registerEdge(this);
    _to->registerEdge(this);
}

SeerParallelStacksLiveEdge::~SeerParallelStacksLiveEdge() {

    // If our endpoints are still alive, tell them to forget about us so
    // they don't later call update() on a dangling SeerParallelStacksLiveEdge* during
    // itemChange(). detachEndpoint() is a no-op if the endpoint has
    // already nulled itself out via SeerParallelStacksStackBoxItem::~SeerParallelStacksStackBoxItem().
    if (_from) _from->unregisterEdge(this);
    if (_to)   _to->unregisterEdge(this);
}

void SeerParallelStacksLiveEdge::detachEndpoint(SeerParallelStacksStackBoxItem* box) {

    if (_from == box) _from = nullptr;
    if (_to   == box) _to   = nullptr;
}

QRectF SeerParallelStacksLiveEdge::boundingRect() const {

    // Return the bounding rect of the two endpoints plus generous padding
    // so the bezier and arrowhead are never clipped.
    if (!_from || !_to) return QRectF();

    QPointF f = _from->sceneBottom();
    QPointF t = _to->sceneTop();

    qreal pad = kArrow + kVCtrl + 4;

    return QRectF(f, t).normalized().adjusted(-pad, -pad, pad, pad);
}

QPainterPath SeerParallelStacksLiveEdge::shape() const {

    // Empty path == this item is never returned by hit-testing
    // (QGraphicsScene::itemAt, collidingItems, etc.). boundingRect()
    // above is intentionally large (it just needs to avoid clipping the
    // curve/arrowhead during paint), so without this override the edge
    // would otherwise swallow clicks meant for boxes or the background.
    return QPainterPath();
}

void SeerParallelStacksLiveEdge::paint(QPainter* painter, const QStyleOptionGraphicsItem*, QWidget*) {

    if (!_from || !_to) return;

    painter->setRenderHint(QPainter::Antialiasing);

    QPointF from = _from->sceneBottom();
    QPointF to   = _to->sceneTop();

    /*
    // Bezier: control points pull vertically toward each other
    QPainterPath path;
    path.moveTo(from);
    path.cubicTo(from + QPointF(0,  kVCtrl), to   + QPointF(0, -kVCtrl), to);

    painter->setPen(QPen(QColor(0x55, 0x55, 0x55), 1.5));
    painter->setBrush(Qt::NoBrush);
    painter->drawPath(path);

    // Arrowhead at `to` pointing downward (into the parent box)
    // The tangent direction at the end of the cubic is (to - cp2)
    QPointF cp2  = to + QPointF(0, -kVCtrl);
    QPointF dir  = to - cp2;
    double  len  = std::hypot(dir.x(), dir.y());
    if (len < 1e-6) return;
    dir /= len;   // normalise

    // Perpendicular
    QPointF perp(-dir.y(), dir.x());

    QPointF a1 = to - dir * kArrow + perp * (kArrow * 0.5);
    QPointF a2 = to - dir * kArrow - perp * (kArrow * 0.5);

    QPolygonF arrowHead;
    arrowHead << to << a1 << a2;
    painter->setPen(Qt::NoPen);
    painter->setBrush(QColor(0x55, 0x55, 0x55));
    painter->drawPolygon(arrowHead);
    */

    // --- Arrowhead: tip touches the child box bottom (`from`),
    //     base is kArrow further along toward `to`.
    //     The bezier starts at the base so the line never overlaps the head.
    QPointF delta = to - from;
    double  len   = std::hypot(delta.x(), delta.y());

    if (len < 1e-6) return;

    QPointF dir  = delta / len;           // unit vector from→to
    QPointF perp(-dir.y(), dir.x());      // perpendicular

    QPointF tip  = from;                  // touches child box
    QPointF base = from + dir * kArrow;   // where the line begins

    QPointF a1 = base + perp * (kArrow * 0.5);
    QPointF a2 = base - perp * (kArrow * 0.5);

    // Bezier runs from the arrowhead base to the parent box top.
    QPainterPath path;

    path.moveTo(base);
    path.cubicTo(base + QPointF(0,  kVCtrl), to   + QPointF(0, -kVCtrl), to);

    painter->setPen(QPen(QColor(0x55, 0x55, 0x55), 1.5));
    painter->setBrush(Qt::NoBrush);
    painter->drawPath(path);

    QPolygonF arrowHead;
    arrowHead << tip << a1 << a2;
    painter->setPen(Qt::NoPen);
    painter->setBrush(QColor(0x55, 0x55, 0x55));
    painter->drawPolygon(arrowHead);
}

// ================================================================
// SeerParallelStacksMiniMapWidget
// ================================================================
SeerParallelStacksMiniMapWidget::SeerParallelStacksMiniMapWidget(SeerParallelStacksGraphicsView* view, QWidget* parent) : QWidget(parent) , _view(view) {

    setAttribute(Qt::WA_NoSystemBackground, false);
    setCursor(Qt::PointingHandCursor);
    setToolTip(QStringLiteral("Click or drag to jump around the graph"));
    resize(sizeHint());
}

QRectF SeerParallelStacksMiniMapWidget::contentRect() const {

    // Inset by a small margin so the border isn't drawn flush against
    // the widget edge.
    return QRectF(rect()).adjusted(2, 2, -2, -2);
}

void SeerParallelStacksMiniMapWidget::refresh() {

    update();
}

void SeerParallelStacksMiniMapWidget::paintEvent(QPaintEvent* ) {

    QPainter painter(this);
    painter.setRenderHint(QPainter::Antialiasing);

    // Panel background
    painter.setBrush(QColor(0xFF, 0xFF, 0xFF, 230));
    painter.setPen(QPen(QColor(0x88, 0x88, 0x88), 1.5));
    painter.drawRoundedRect(rect().adjusted(0, 0, -1, -1), 6, 6);

    if (!_view || !_view->scene()) {
        return;
    }

    const QRectF sceneRect = _view->sceneRect();

    if (sceneRect.isEmpty()) {
        return;
    }

    const QRectF avail = contentRect();

    // Compute a uniform scale that fits the whole scene into the
    // available area while preserving aspect ratio, then centre it.
    const qreal sx = avail.width()  / sceneRect.width();
    const qreal sy = avail.height() / sceneRect.height();
    const qreal s  = std::min(sx, sy);

    const qreal drawW = sceneRect.width()  * s;
    const qreal drawH = sceneRect.height() * s;
    const QPointF origin(avail.left() + (avail.width() - drawW) / 2.0, avail.top() + (avail.height() - drawH) / 2.0);

    auto sceneToWidget = [&](const QPointF& p) -> QPointF {
        return origin + QPointF((p.x() - sceneRect.left()) * s, (p.y() - sceneRect.top()) * s);
    };

    // Draw a small marker for every SeerParallelStacksStackBoxItem so the overall shape
    // of the graph is recognisable at a glance.
    painter.setPen(Qt::NoPen);
    painter.setBrush(QColor(0x5A, 0x8F, 0xD6));

    for (QGraphicsItem* item : _view->scene()->items()) {
        if (auto* box = dynamic_cast<SeerParallelStacksStackBoxItem*>(item)) {
            QRectF r(sceneToWidget(box->sceneBoundingRect().topLeft()), sceneToWidget(box->sceneBoundingRect().bottomRight()));
            if (r.width() < 2) r.setWidth(2);
            if (r.height() < 2) r.setHeight(2);
            painter.drawRoundedRect(r, 1, 1);
        }
    }

    // Draw the main view's current visible region as an outlined box.
    const QRectF visibleScene = _view->mapToScene(_view->viewport()->rect()).boundingRect();

    QRectF viewportMarker(sceneToWidget(visibleScene.topLeft()), sceneToWidget(visibleScene.bottomRight()));

    painter.setBrush(QColor(0x1A, 0x52, 0xA8, 40));
    painter.setPen(QPen(QColor(0x1A, 0x52, 0xA8), 1.5));
    painter.drawRect(viewportMarker);
}

void SeerParallelStacksMiniMapWidget::jumpToWidgetPos(const QPoint& widgetPos) {

    if (!_view || !_view->scene()) return;

    const QRectF sceneRect = _view->sceneRect();

    if (sceneRect.isEmpty()) return;

    const QRectF avail = contentRect();
    const qreal sx = avail.width()  / sceneRect.width();
    const qreal sy = avail.height() / sceneRect.height();
    const qreal s  = std::min(sx, sy);

    const qreal drawW = sceneRect.width()  * s;
    const qreal drawH = sceneRect.height() * s;
    const QPointF origin(avail.left() + (avail.width() - drawW) / 2.0, avail.top() + (avail.height() - drawH) / 2.0);

    // Inverse of sceneToWidget()
    const QPointF scenePos( sceneRect.left() + (widgetPos.x() - origin.x()) / s, sceneRect.top()  + (widgetPos.y() - origin.y()) / s);

    _view->centerOn(scenePos);

    refresh();
}

void SeerParallelStacksMiniMapWidget::mousePressEvent(QMouseEvent* event) {

    if (event->button() == Qt::LeftButton) {
        _dragging = true;
        jumpToWidgetPos(event->pos());
        event->accept();
        return;
    }

    QWidget::mousePressEvent(event);
}

void SeerParallelStacksMiniMapWidget::mouseMoveEvent(QMouseEvent* event) {

    if (_dragging) {
        jumpToWidgetPos(event->pos());
        event->accept();
        return;
    }

    QWidget::mouseMoveEvent(event);
}

void SeerParallelStacksMiniMapWidget::mouseReleaseEvent(QMouseEvent* event) {

    if (_dragging && event->button() == Qt::LeftButton) {
        _dragging = false;
        event->accept();
        return;
    }

    QWidget::mouseReleaseEvent(event);
}

// ================================================================
// SeerParallelStacksPopupTableWidget
// ================================================================
SeerParallelStacksPopupTableWidget::SeerParallelStacksPopupTableWidget(QWidget* parent) : QFrame(parent) {

    // Frameless, no taskbar entry. Using Qt::Window instead of Qt::Popup
    // here because Qt::Popup grabs the mouse and can interfere with
    // leaveEvent delivery on some platforms.
    setWindowFlags(Qt::Window | Qt::FramelessWindowHint | Qt::WindowStaysOnTopHint);

    // Visible frame around the table
    setFrameShape(QFrame::Box);
    setFrameShadow(QFrame::Plain);
    setLineWidth(1);
    setStyleSheet(
            "SeerParallelStacksPopupTableWidget {"
            "  background-color: palette(window);"
            "  border: 1px solid #888;"
            "  border-radius: 4px;"
            "}"
            );

    // Ensure leaveEvent fires reliably even without buttons held
    setMouseTracking(true);

    _table = new QTableWidget(this);
    _table->setRowCount(0);
    _table->setColumnCount(2);
    _table->horizontalHeader()->setDefaultAlignment(Qt::AlignLeft);
    _table->setHorizontalHeaderLabels({"ThreadID","Function"});
    _table->setHorizontalScrollBarPolicy(Qt::ScrollBarAsNeeded);
    _table->setHorizontalScrollMode(QAbstractItemView::ScrollPerPixel);
    _table->horizontalScrollBar()->setSingleStep(5);
    _table->setVerticalScrollBarPolicy(Qt::ScrollBarAsNeeded);
    _table->setMouseTracking(true);
    _table->viewport()->setMouseTracking(true);
    _table->horizontalHeader()->setStretchLastSection(true);
    _table->verticalHeader()->setVisible(false);
    _table->setEditTriggers(QAbstractItemView::DoubleClicked); // NoEditTriggers
    _table->setSelectionBehavior(QAbstractItemView::SelectRows);
    _table->setFrameShape(QFrame::NoFrame); // outer QFrame provides the border
    _table->resizeColumnToContents(0);
    _table->resizeColumnToContents(1);

    // Small padding between the outer frame border and the table itself
    QVBoxLayout* layout = new QVBoxLayout(this);
    layout->setContentsMargins(6, 6, 6, 6);
    layout->addWidget(_table);

    _closeTimer = new QTimer(this);
    _closeTimer->setSingleShot(true); // Ensures it only fires once
    _closeTimer->setInterval(1500);   // Delay

    // Connect things.
    QObject::connect(_closeTimer, &QTimer::timeout,      this, &SeerParallelStacksPopupTableWidget::handleCloseTimer);

    _closeTimer->start();
}

void SeerParallelStacksPopupTableWidget::addRow (int threadid, const QString& function) {

    int nrows = _table->rowCount();

    _table->setRowCount(nrows+1);

    QTableWidgetItem* item0 = new QTableWidgetItem(QString::number(threadid));
    item0->setTextAlignment(Qt::AlignRight | Qt::AlignVCenter);
    item0->setFlags(item0->flags()|Qt::ItemIsEditable);

    QTableWidgetItem* item1 = new QTableWidgetItem(function);
    item1->setTextAlignment(Qt::AlignLeft | Qt::AlignVCenter);
    item1->setFlags(item1->flags()|Qt::ItemIsEditable);

    _table->setItem(nrows, 0, item0);
    _table->setItem(nrows, 1, item1);

    _table->resizeColumnToContents(0);
    _table->resizeColumnToContents(1);
}

void SeerParallelStacksPopupTableWidget::enterEvent(QEnterEvent* event) {

    // If the user moves the cursor into the table, cancel the timer.
    if (_closeTimer->isActive()) {
        _closeTimer->stop();
    }

    QFrame::enterEvent(event);
}

void SeerParallelStacksPopupTableWidget::leaveEvent(QEvent* event) {

    // If the user leaves the table, ask for the table to be closed.
    QFrame::leaveEvent(event);

    emit mouseLeftPopup();
}

void SeerParallelStacksPopupTableWidget::handleCloseTimer() {

    // If the user doesn't move the cursor into the table in the default time,
    // ask the table to be closed.
    emit mouseLeftPopup();
}

// ================================================================
// SeerParallelStacksPopupTableWidget
// ================================================================
constexpr qreal kHGap = 30.0;
constexpr qreal kVGap = 60.0;

SeerParallelStacksGraphicsView::SeerParallelStacksGraphicsView(QWidget* parent) : QGraphicsView(parent), _scene(new QGraphicsScene(this)) {

    setScene(_scene);
    setRenderHint(QPainter::Antialiasing);
    setDragMode(QGraphicsView::NoDrag);
    setTransformationAnchor(QGraphicsView::AnchorUnderMouse);
    setBackgroundBrush(QColor(0xF0, 0xF0, 0xF0));
    setHorizontalScrollBarPolicy(Qt::ScrollBarAsNeeded);
    setVerticalScrollBarPolicy(Qt::ScrollBarAsNeeded);

    _miniMap = new SeerParallelStacksMiniMapWidget(this, this);
    _miniMap->raise();


    // Connect things.
    QObject::connect(_scene, &QGraphicsScene::changed,      this,     &SeerParallelStacksGraphicsView::handleGrowSceneRectToFitItems);
    QObject::connect(_scene, &QGraphicsScene::changed,      _miniMap, &SeerParallelStacksMiniMapWidget::refresh);
}

void SeerParallelStacksGraphicsView::wheelEvent(QWheelEvent* event) {

    const double factor = event->angleDelta().y() > 0 ? 1.15 : 1.0 / 1.15;

    scale(factor, factor);

    if (_miniMap) _miniMap->refresh();
}

void SeerParallelStacksGraphicsView::keyPressEvent(QKeyEvent* event) {

    if (event->key() == Qt::Key_Shift) {
        setDragMode(QGraphicsView::ScrollHandDrag);
    }

    QGraphicsView::keyPressEvent(event);
}

void SeerParallelStacksGraphicsView::keyReleaseEvent(QKeyEvent* event) {

    if (event->key() == Qt::Key_Shift) {
        setDragMode(QGraphicsView::NoDrag);
    }

    QGraphicsView::keyReleaseEvent(event);
}


void SeerParallelStacksGraphicsView::resizeEvent(QResizeEvent* event) {

    QGraphicsView::resizeEvent(event);

    repositionMiniMap();
}

void SeerParallelStacksGraphicsView::scrollContentsBy(int dx, int dy) {

    QGraphicsView::scrollContentsBy(dx, dy);

    if (_miniMap) _miniMap->refresh();
}

void SeerParallelStacksGraphicsView::repositionMiniMap() {

    if (!_miniMap) return;

    const QSize s = _miniMap->sizeHint();
    const int margin = 10;

    _miniMap->setGeometry(width() - s.width() - margin, height() - s.height() - margin, s.width(), s.height());
}

void SeerParallelStacksGraphicsView::mousePressEvent(QMouseEvent* event) {

    // Background panning only kicks in for Ctrl+LMB on empty space.
    // If the click landed on an item (e.g. a SeerParallelStacksStackBoxItem), let the base
    // class dispatch it to that item — SeerParallelStacksStackBoxItem handles its own
    // Ctrl+LMB grab-and-move behavior.
    if (event->button() == Qt::LeftButton && event->modifiers() & Qt::ControlModifier && itemAt(event->pos()) == nullptr) {
        _panning     = true;
        _panStartPos = event->pos();
        setCursor(Qt::ClosedHandCursor);
        event->accept();
        return;
    }

    QGraphicsView::mousePressEvent(event);
}

void SeerParallelStacksGraphicsView::mouseMoveEvent(QMouseEvent* event) {

    if (_panning) {
        const QPoint delta = event->pos() - _panStartPos;
        _panStartPos = event->pos();
        horizontalScrollBar()->setValue(horizontalScrollBar()->value() - delta.x());
        verticalScrollBar()->setValue(verticalScrollBar()->value() - delta.y());
        event->accept();
        return;
    }

    QGraphicsView::mouseMoveEvent(event);
}

void SeerParallelStacksGraphicsView::mouseReleaseEvent(QMouseEvent* event) {

    if (_panning && event->button() == Qt::LeftButton) {
        _panning = false;
        setCursor(Qt::ArrowCursor);
        event->accept();
        return;
    }

    QGraphicsView::mouseReleaseEvent(event);
}


void SeerParallelStacksGraphicsView::setStack(const std::shared_ptr<SeerParallelStacksStack>& root) {

    _scene->clear();

    if (!root) return;

    auto* rootPN = new PlacedNode;
    buildPlacedTree(rootPN, root, nullptr);

    qreal xCursor = 0;
    layoutTree(rootPN, xCursor, 0);

    // --- Align parentless items to a shared bottom row ---
    // After layout, children sit above (smaller Y) their parents.
    // Nodes whose PlacedNode parent has no item are the visual roots —
    // they have nothing above them. We push them all to the same bottom
    // Y so they form a flush baseline at the deepest point in the scene.

    // 1. Find the maximum bottom edge across every placed item.
    qreal maxBottom = 0;
    collectMaxBottom(rootPN, maxBottom);

    // 2. Shift each parentless item so its bottom aligns to maxBottom.
    alignParentlessToBottom(rootPN, maxBottom);

    addEdges(rootPN);
    deleteTree(rootPN);

    // Let the scene rect track item bounds dynamically (Qt recomputes it
    // automatically as items move/grow). A fixed rect set once here would
    // go stale once items are dragged via Ctrl+LMB, leaving the scrollbar
    // range clamped to the original extents at the original zoom level.
    _scene->setSceneRect(QRectF());

    // Fit the (current) scene contents into the viewport once, then capture
    // that as the explicit scene rect with some padding so scrollbars have
    // a stable, generously-sized range to work with at any zoom level.
    QRectF bounds = _scene->itemsBoundingRect().adjusted(-40, -40, 40, 40);

    _scene->setSceneRect(bounds);

    fitInView(bounds, Qt::KeepAspectRatio);

    repositionMiniMap();

    if (_miniMap) _miniMap->refresh();
}

// Recursively find the maximum bottom edge (item->y() + item->height()) in the tree.
void SeerParallelStacksGraphicsView::collectMaxBottom(PlacedNode* pn, qreal& maxBottom) {

    if (pn->item) {
        maxBottom = std::max(maxBottom, pn->item->y() + pn->item->height());
    }

    for (auto* child : pn->children) {
        collectMaxBottom(child, maxBottom);
    }
}

// Shift every parentless item (direct visual root — parent has no box)
// downward so its bottom edge sits at maxBottom.
void SeerParallelStacksGraphicsView::alignParentlessToBottom(PlacedNode* pn, qreal maxBottom) {

    for (auto* child : pn->children) {
        // child->parent == rootPN (which has no item), so child is parentless.
        if (child->item) {
            qreal currentBottom = child->item->y() + child->item->height();
            qreal dy = maxBottom - currentBottom;
            if (std::abs(dy) > 0.5) {
                child->item->setPos(child->item->x(), child->item->y() + dy);
            }
        }
        // Do NOT recurse — only top-level parentless nodes are shifted.
    }
}

void SeerParallelStacksGraphicsView::buildPlacedTree(PlacedNode* pn, const std::shared_ptr<SeerParallelStacksStack>& stack, PlacedNode* parentPN) {

    pn->stack  = stack;
    pn->parent = parentPN;

    if (!stack->functions.isEmpty()) {
        pn->item = new SeerParallelStacksStackBoxItem(*stack);
        _scene->addItem(pn->item);
    }

    for (const auto& child : stack->stacks) {
        auto* childPN = new PlacedNode;
        buildPlacedTree(childPN, child, pn);
        pn->children.append(childPN);
    }
}

void SeerParallelStacksGraphicsView::layoutTree(PlacedNode* pn, qreal& xCursor, qreal yTop) {

    qreal itemW = pn->item ? pn->item->width()  : 0;
    qreal itemH = pn->item ? pn->item->height() : 0;

    if (pn->children.isEmpty()) {
        pn->cx = xCursor + itemW / 2.0;
        if (pn->item) {
            pn->item->setPos(xCursor, yTop);
            pn->cy = yTop + itemH;
        }
        xCursor += itemW + kHGap;
    }else{
        qreal childY  = yTop;
        qreal firstCx = -1, lastCx = -1;

        for (auto* child : pn->children) {
            layoutTree(child, xCursor, childY);
            if (firstCx < 0) firstCx = child->cx;
            lastCx = child->cx;
        }

        pn->cx = (firstCx + lastCx) / 2.0;

        qreal maxChildBottom = childY;

        for (auto* child : pn->children) {
            maxChildBottom = std::max(maxChildBottom, child->cy);
        }

        qreal parentY = maxChildBottom + kVGap;

        if (pn->item) {
            pn->item->setPos(pn->cx - itemW / 2.0, parentY);
            pn->cy = parentY + itemH;
        }else{
            pn->cy = parentY;
        }
    }
}

void SeerParallelStacksGraphicsView::addEdges(PlacedNode* pn) {

    for (auto* child : pn->children) {
        if (pn->item && child->item) {
            // SeerParallelStacksLiveEdge registers itself with both endpoints on construction.
            // The scene takes ownership via addItem.
            auto* edge = new SeerParallelStacksLiveEdge(child->item, pn->item);
            _scene->addItem(edge);
        }
        addEdges(child);
    }
}

void SeerParallelStacksGraphicsView::deleteTree(PlacedNode* pn) {

    for (auto* child : pn->children) {
        deleteTree(child);
    }

    delete pn;
}

void SeerParallelStacksGraphicsView::handleGrowSceneRectToFitItems() {

    if (_scene->items().isEmpty()) {
        return;
    }

    const QRectF current = _scene->sceneRect();
    const QRectF needed  = _scene->itemsBoundingRect().adjusted(-40, -40, 40, 40);

    // Only grow — never shrink on every change, to avoid jitter — and only
    // when the items actually fall outside the current rect.
    if (!current.contains(needed)) {
        _scene->setSceneRect(current.united(needed));
    }
}

