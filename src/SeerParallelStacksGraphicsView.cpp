#include "SeerParallelStacksGraphicsView.h"
#include <QPainter>
#include <QPainterPath>
#include <QWheelEvent>
#include <QFontMetrics>
#include <QGraphicsSceneMouseEvent>
#include <QCursor>
#include <algorithm>
#include <cmath>

namespace Seer::PSV {

    // ================================================================
    // StackBoxItem
    // ================================================================

    StackBoxItem::StackBoxItem(const Stack& stack, QGraphicsItem* parent) : QGraphicsItem(parent) {

        // Enable geometry-change notifications so itemChange() fires on setPos()
        setFlag(QGraphicsItem::ItemSendsGeometryChanges, true);

        _headerLeft = QString("%1 Thread%2") .arg(stack.threadCount) .arg(stack.threadCount == 1 ? "" : "s");

        if (!stack.threadIds.isEmpty()) {
            QStringList ids = stack.threadIds;
            if (ids.size() > 8)
                _headerRight = QString("[%1 … +%2]") .arg(ids.mid(0, 8).join(", ")) .arg(ids.size() - 8);
            else
                _headerRight = "[" + ids.join(", ") + "]";
        }

        for (int i = stack.functions.size() - 1; i >= 0; --i) {
            _rows.append({ stack.functions[i], Function });
        }

        QFont        boldFont;  boldFont.setBold(true);
        QFontMetrics boldFm(boldFont);
        QFont        normFont;
        QFontMetrics normFm(normFont);

        qreal headerW = boldFm.horizontalAdvance(_headerLeft)
            + boldFm.horizontalAdvance(_headerRight)
            + kHeaderGap;
        qreal maxTextW = headerW;

        for (const auto& row : _rows) {
            maxTextW = std::max(maxTextW, (qreal)normFm.horizontalAdvance(row.text));
        }

        _width  = maxTextW + 2 * kPadX;
        _height = kPadY + kRowH * (1 + (int)_rows.size()) + kPadY;
    }

    QRectF StackBoxItem::boundingRect() const {
        return QRectF(0, 0, _width, _height);
    }

    void StackBoxItem::paint(QPainter* painter, const QStyleOptionGraphicsItem*, QWidget*) {

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

    QPointF StackBoxItem::sceneBottom() const {
        return mapToScene(QPointF(_width / 2.0, _height));
    }

    QPointF StackBoxItem::sceneTop() const {
        return mapToScene(QPointF(_width / 2.0, 0));
    }

    QVariant StackBoxItem::itemChange(GraphicsItemChange change, const QVariant& value) {

        if (change == ItemPositionHasChanged) {
            for (LiveEdge* e : _edges) {
                e->update();   // ask each connected edge to repaint
            }
        }

        return QGraphicsItem::itemChange(change, value);
    }

    void StackBoxItem::mousePressEvent(QGraphicsSceneMouseEvent* event) {

        if (event->button() == Qt::LeftButton && event->modifiers() & Qt::ControlModifier) {

            _dragging   = true;
            _dragOffset = event->pos();

            setCursor(Qt::ClosedHandCursor);
            setZValue(10);
            update();

            event->accept();

        }else{
            QGraphicsItem::mousePressEvent(event);
        }
    }

    void StackBoxItem::mouseMoveEvent(QGraphicsSceneMouseEvent* event) {
        if (_dragging) {
            setPos(mapToScene(event->pos() - _dragOffset));
            event->accept();
        }else{
            QGraphicsItem::mouseMoveEvent(event);
        }
    }

    void StackBoxItem::mouseReleaseEvent(QGraphicsSceneMouseEvent* event) {
        if (_dragging && event->button() == Qt::LeftButton) {

            _dragging = false;

            setCursor(Qt::ArrowCursor);
            setZValue(0);
            update();

            event->accept();

        }else{
            QGraphicsItem::mouseReleaseEvent(event);
        }
    }

    // ================================================================
    // LiveEdge — redraws itself each paint() from current endpoint positions
    // ================================================================

    LiveEdge::LiveEdge(StackBoxItem* from, StackBoxItem* to, QGraphicsItem* parent) : QGraphicsItem(parent) , _from(from) , _to(to) {

        setZValue(-1);
        // Position the edge item at the scene origin; all coordinates are scene-space.
        setPos(0, 0);

        _from->registerEdge(this);
        _to->registerEdge(this);
    }

    LiveEdge::~LiveEdge() {
        // Guard against half-destroyed scenes
        if (_from) _from->unregisterEdge(this);
        if (_to)   _to->unregisterEdge(this);
    }

    QRectF LiveEdge::boundingRect() const {

        // Return the bounding rect of the two endpoints plus generous padding
        // so the bezier and arrowhead are never clipped.
        if (!_from || !_to) return QRectF();

        QPointF f = _from->sceneBottom();
        QPointF t = _to->sceneTop();

        qreal pad = kArrow + kVCtrl + 4;

        return QRectF(f, t).normalized().adjusted(-pad, -pad, pad, pad);
    }

    void LiveEdge::paint(QPainter* painter, const QStyleOptionGraphicsItem*, QWidget*) {

        if (!_from || !_to) return;

        painter->setRenderHint(QPainter::Antialiasing);

        QPointF from = _from->sceneBottom();
        QPointF to   = _to->sceneTop();

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
    }
}

// ================================================================
// SeerParallelStacksGraphicsView
// ================================================================

using Seer::PSV::StackBoxItem;
using Seer::PSV::LiveEdge;

constexpr qreal kHGap = 30.0;
constexpr qreal kVGap = 60.0;

SeerParallelStacksGraphicsView::SeerParallelStacksGraphicsView(QWidget* parent) : QGraphicsView(parent) , _scene(new QGraphicsScene(this)) {

    setScene(_scene);
    setRenderHint(QPainter::Antialiasing);
    setDragMode(QGraphicsView::ScrollHandDrag);
    setTransformationAnchor(QGraphicsView::AnchorUnderMouse);
    setBackgroundBrush(QColor(0xF0, 0xF0, 0xF0));
    setHorizontalScrollBarPolicy(Qt::ScrollBarAsNeeded);
    setVerticalScrollBarPolicy(Qt::ScrollBarAsNeeded);

    // Connect things.
    QObject::connect(_scene, &QGraphicsScene::changed,      this, &SeerParallelStacksGraphicsView::handleGrowSceneRectToFitItems);
}

void SeerParallelStacksGraphicsView::wheelEvent(QWheelEvent* event) {

    const double factor = event->angleDelta().y() > 0 ? 1.15 : 1.0 / 1.15;

    scale(factor, factor);
}

void SeerParallelStacksGraphicsView::setStack(const std::shared_ptr<Seer::PSV::Stack>& root) {

    _scene->clear();

    if (!root) return;

    auto *rootPN = new PlacedNode;
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

void SeerParallelStacksGraphicsView::buildPlacedTree(PlacedNode* pn, const std::shared_ptr<Seer::PSV::Stack>& stack, PlacedNode* parentPN) {

    pn->stack  = stack;
    pn->parent = parentPN;

    if (!stack->functions.isEmpty()) {
        pn->item = new StackBoxItem(*stack);
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
            // LiveEdge registers itself with both endpoints on construction.
            // The scene takes ownership via addItem.
            auto* edge = new LiveEdge(child->item, pn->item);
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

