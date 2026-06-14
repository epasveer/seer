#pragma once
#include "SeerParallelStacksCommon.h"
#include <QWidget>
#include <QGraphicsView>
#include <QGraphicsScene>
#include <QGraphicsItem>
#include <QPointF>
#include <QVector>

namespace Seer {
namespace PSV {

class LiveEdge;   // forward — StackBoxItem needs to know it

// ---------------------------------------------------------------
// A single box in the graph: shows thread count + IDs + call frames.
// Ctrl+LMB grabs and moves the item freely within the scene.
// Moving the item automatically redraws all connected LiveEdges.
// ---------------------------------------------------------------
class StackBoxItem : public QGraphicsItem
{
public:
    explicit StackBoxItem(const Stack &stack, QGraphicsItem *parent = nullptr);

    QRectF boundingRect() const override;
    void   paint(QPainter *painter,
                 const QStyleOptionGraphicsItem *option,
                 QWidget *widget) override;

    qreal width()  const { return m_width;  }
    qreal height() const { return m_height; }

    // Edge registry — called by LiveEdge on construction/destruction
    void registerEdge  (LiveEdge *e) { m_edges.append(e); }
    void unregisterEdge(LiveEdge *e) { m_edges.removeAll(e); }

    // Bottom-centre and top-centre in scene coordinates (edge attach points)
    QPointF sceneBottom() const;
    QPointF sceneTop()    const;

protected:
    QVariant itemChange(GraphicsItemChange change, const QVariant &value) override;

    void mousePressEvent  (QGraphicsSceneMouseEvent *event) override;
    void mouseMoveEvent   (QGraphicsSceneMouseEvent *event) override;
    void mouseReleaseEvent(QGraphicsSceneMouseEvent *event) override;

private:
    enum RowKind { Function };
    struct Row { QString text; RowKind kind; };

    QString      m_headerLeft;
    QString      m_headerRight;
    QVector<Row> m_rows;
    qreal        m_width  = 0;
    qreal        m_height = 0;

    bool    m_dragging  = false;
    QPointF m_dragOffset;

    QVector<LiveEdge *> m_edges;   // non-owning

    static constexpr qreal kPadX      = 12;
    static constexpr qreal kPadY      =  8;
    static constexpr qreal kRowH      = 20;
    static constexpr qreal kHeaderGap = 16;
};

// ---------------------------------------------------------------
// A live bezier edge between two StackBoxItems.
// It redraws itself whenever either endpoint moves.
// ---------------------------------------------------------------
class LiveEdge : public QGraphicsItem
{
public:
    LiveEdge(StackBoxItem *from, StackBoxItem *to, QGraphicsItem *parent = nullptr);
    ~LiveEdge() override;

    QRectF boundingRect() const override;
    void   paint(QPainter *painter,
                 const QStyleOptionGraphicsItem *option,
                 QWidget *widget) override;

private:
    StackBoxItem *m_from;   // child  (bottom anchor)
    StackBoxItem *m_to;     // parent (top anchor)

    static constexpr qreal kArrow = 8.0;
    static constexpr qreal kVCtrl = 60.0 * 0.4;   // bezier control-point stretch
};

} // namespace PSV
} // namespace Seer

// ---------------------------------------------------------------
// The full graph view.  Scroll-drag (no modifier) pans the canvas.
// ---------------------------------------------------------------
class SeerParallelStacksGraphicsView : public QGraphicsView
{
    Q_OBJECT
public:
    explicit SeerParallelStacksGraphicsView(QWidget *parent = nullptr);
    void setStack(const std::shared_ptr<Seer::PSV::Stack> &root);

protected:
    void wheelEvent(QWheelEvent *event) override;

private:
    struct PlacedNode {
        std::shared_ptr<Seer::PSV::Stack>        stack;
        Seer::PSV::StackBoxItem      *item   = nullptr;
        PlacedNode                   *parent = nullptr;
        QVector<PlacedNode *>         children;
        qreal                          cx     = 0;
        qreal                          cy     = 0;
    };

    QGraphicsScene *m_scene;

    void buildPlacedTree(PlacedNode *pn, const std::shared_ptr<Seer::PSV::Stack> &stack,
                         PlacedNode *parentPN);
    void layoutTree(PlacedNode *pn, qreal &xCursor, qreal yTop);
    void collectMaxBottom(PlacedNode *pn, qreal &maxBottom);
    void alignParentlessToBottom(PlacedNode *pn, qreal maxBottom);
    void addEdges(PlacedNode *pn);
    void deleteTree(PlacedNode *pn);
};
