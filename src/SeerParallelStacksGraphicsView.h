#pragma once

#include "SeerParallelStacksCommon.h"
#include <QWidget>
#include <QGraphicsView>
#include <QGraphicsScene>
#include <QGraphicsItem>
#include <QPointF>
#include <QVector>

namespace Seer::PSV {

    class LiveEdge;   // forward — StackBoxItem needs to know it

    // ---------------------------------------------------------------
    // A single box in the graph: shows thread count + IDs + call frames.
    // Ctrl+LMB grabs and moves the item freely within the scene.
    // Moving the item automatically redraws all connected LiveEdges.
    // ---------------------------------------------------------------
    class StackBoxItem : public QGraphicsItem {

        public:
            explicit StackBoxItem(const Stack& stack, QGraphicsItem* parent = nullptr);
           ~StackBoxItem() override;

            QRectF                  boundingRect        () const override;
            void                    paint               (QPainter* painter, const QStyleOptionGraphicsItem* option, QWidget* widget) override;

            qreal                   width               () const { return _width;  }
            qreal                   height              () const { return _height; }

            // Edge registry — called by LiveEdge on construction/destruction
            void                    registerEdge        (LiveEdge* e) { _edges.append(e); }
            void                    unregisterEdge      (LiveEdge* e) { _edges.removeAll(e); }

            // Bottom-centre and top-centre in scene coordinates (edge attach points)
            QPointF                 sceneBottom         () const;
            QPointF                 sceneTop            () const;

        protected:
            QVariant                itemChange          (GraphicsItemChange change, const QVariant& value) override;

            void                    mousePressEvent     (QGraphicsSceneMouseEvent* event) override;
            void                    mouseMoveEvent      (QGraphicsSceneMouseEvent* event) override;
            void                    mouseReleaseEvent   (QGraphicsSceneMouseEvent* event) override;
            void                    hoverEnterEvent     (QGraphicsSceneHoverEvent* event) override;
            void                    hoverLeaveEvent     (QGraphicsSceneHoverEvent* event) override;

        private:
            enum RowKind { Function };
            struct Row { QString text; RowKind kind; };

            QString                 _headerLeft;
            QString                 _headerRight;
            QVector<Row>            _rows;
            qreal                   _width  = 0;
            qreal                   _height = 0;

            bool                    _dragging  = false;
            QPointF                 _dragOffset;

            QVector<LiveEdge*>      _edges;   // non-owning

            static constexpr qreal  kPadX      = 12;
            static constexpr qreal  kPadY      =  8;
            static constexpr qreal  kRowH      = 20;
            static constexpr qreal  kHeaderGap = 16;
    };

    // ---------------------------------------------------------------
    // A live bezier edge between two StackBoxItems.
    // It redraws itself whenever either endpoint moves.
    // ---------------------------------------------------------------
    class LiveEdge : public QGraphicsItem {

        public:
            LiveEdge(StackBoxItem* from, StackBoxItem* to, QGraphicsItem* parent = nullptr);
           ~LiveEdge() override;

            QRectF boundingRect     () const override;
            void   paint            (QPainter* painter, const QStyleOptionGraphicsItem* option, QWidget* widget) override;

            // Called by StackBoxItem's destructor so this edge stops referencing
            // an endpoint that is about to be (or has been) destroyed. After this,
            // the edge renders nothing and its own destructor won't touch `box`.
            void   detachEndpoint   (StackBoxItem* box);


        private:
            StackBoxItem*           _from;   // child  (bottom anchor)
            StackBoxItem*           _to;     // parent (top anchor)

            static constexpr qreal  kArrow = 8.0;
            static constexpr qreal  kVCtrl = 60.0 * 0.4;   // bezier control-point stretch
    };

}

class SeerParallelStacksGraphicsView;

// ---------------------------------------------------------------
// Small overlay widget showing the entire scene at a glance, with a
// rectangle marking the main view's current visible region. Click or
// drag inside it to jump/pan the main view there.
// ---------------------------------------------------------------
class MiniMapWidget : public QWidget {

    Q_OBJECT

    public:
        explicit MiniMapWidget(SeerParallelStacksGraphicsView* view, QWidget* parent = nullptr);

        QSize   sizeHint                () const override { return QSize(180, 140); }

    public slots:
        void    refresh                 ();   // call whenever the scene or the main view's viewport changes

    protected:
        void    paintEvent              (QPaintEvent* event) override;
        void    mousePressEvent         (QMouseEvent* event) override;
        void    mouseMoveEvent          (QMouseEvent* event) override;
        void    mouseReleaseEvent       (QMouseEvent* event) override;

    private:
        // Maps a point in this widget to a scene position, and centers the
        // main view there.
        void    jumpToWidgetPos         (const QPoint& widgetPos);

        // The scaled rect (within this widget) that the minimap content is
        // drawn into, preserving the scene's aspect ratio.
        QRectF  contentRect             () const;

        SeerParallelStacksGraphicsView* _view;
        bool                            _dragging = false;
};


class SeerParallelStacksGraphicsView : public QGraphicsView {

    Q_OBJECT

    public:
        explicit SeerParallelStacksGraphicsView(QWidget* parent = nullptr);

        void            setStack                        (const std::shared_ptr<Seer::PSV::Stack>& root);

    protected:
        void            wheelEvent                      (QWheelEvent* event) override;
        void            keyPressEvent                   (QKeyEvent* event) override;
        void            keyReleaseEvent                 (QKeyEvent* event) override;
        void            mousePressEvent                 (QMouseEvent* event) override;
        void            mouseMoveEvent                  (QMouseEvent* event) override;
        void            mouseReleaseEvent               (QMouseEvent* event) override;
        void            resizeEvent                     (QResizeEvent* event) override;
        void            scrollContentsBy                (int dx, int dy) override;


    private slots:
        // Grows the scene rect (and thus the scrollbar range) if items have
        // been dragged outside the current bounds. Connected to
        // QGraphicsScene::changed, which fires on item moves/repaints.
        void            handleGrowSceneRectToFitItems   ();

    private:
        struct PlacedNode {
            std::shared_ptr<Seer::PSV::Stack>   stack;
            Seer::PSV::StackBoxItem*            item   = nullptr;
            PlacedNode*                         parent = nullptr;
            QVector<PlacedNode*>                children;
            qreal                               cx     = 0;
            qreal                               cy     = 0;
        };

        void            buildPlacedTree                 (PlacedNode* pn, const std::shared_ptr<Seer::PSV::Stack>& stack, PlacedNode* parentPN);
        void            layoutTree                      (PlacedNode* pn, qreal& xCursor, qreal yTop);
        void            collectMaxBottom                (PlacedNode* pn, qreal& maxBottom);
        void            alignParentlessToBottom         (PlacedNode* pn, qreal maxBottom);
        void            addEdges                        (PlacedNode* pn);
        void            deleteTree                      (PlacedNode* pn);
        void            repositionMiniMap               (); // keeps it pinned to bottom-right corner

        QGraphicsScene* _scene;
        MiniMapWidget*  _miniMap;

        bool            _panning = false;
        QPoint          _panStartPos;  // viewport coords at pan start

        friend class MiniMapWidget;    // needs sceneRect()/mapToScene()/centerOn() access
};

