#pragma once

#include "SeerParallelStacksCommon.h"
#include <QWidget>
#include <QGraphicsView>
#include <QGraphicsScene>
#include <QGraphicsItem>
#include <QTableWidget>
#include <QTimer>
#include <QPointF>
#include <QVector>

class SeerParallelStacksPopupTableWidget;

class SeerParallelStacksLiveEdge;   // forward — SeerParallelStacksStackBoxItem needs to know it

// ---------------------------------------------------------------
// A single box in the graph: shows thread count + IDs + call frames.
// Ctrl+LMB grabs and moves the item freely within the scene.
// Moving the item automatically redraws all connected LiveEdges.
// ---------------------------------------------------------------
class SeerParallelStacksStackBoxItem : public QObject, public QGraphicsItem {

    Q_OBJECT
    Q_INTERFACES(QGraphicsItem)

    public:
        explicit SeerParallelStacksStackBoxItem(const SeerParallelStacksStack& stack, QGraphicsItem* parent = nullptr);
        ~SeerParallelStacksStackBoxItem() override;

        QRectF                  boundingRect        () const override;
        void                    paint               (QPainter* painter, const QStyleOptionGraphicsItem* option, QWidget* widget) override;

        qreal                   width               () const { return _width;  }
        qreal                   height              () const { return _height; }

        // Edge registry — called by LiveEdge on construction/destruction
        void                    registerEdge        (SeerParallelStacksLiveEdge* e) { _edges.append(e); }
        void                    unregisterEdge      (SeerParallelStacksLiveEdge* e) { _edges.removeAll(e); }

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

    private slots:
        void                    handleDeletePopup   ();

    private:
        enum RowKind { Function };
        struct Row { QString text; RowKind kind; };

        QString                                 _headerLeft;
        QString                                 _headerRight;
        QVector<Row>                            _rows;
        qreal                                   _width  = 0;
        qreal                                   _height = 0;
        SeerParallelStacksPopupTableWidget*     _popup  = 0;

        bool                                    _dragging  = false;
        QPointF                                 _dragOffset;

        QVector<SeerParallelStacksLiveEdge*>                      _edges;   // non-owning

        static constexpr qreal  kPadX      = 12;
        static constexpr qreal  kPadY      =  8;
        static constexpr qreal  kRowH      = 20;
        static constexpr qreal  kHeaderGap = 16;
};

// ---------------------------------------------------------------
// A live bezier edge between two StackBoxItems.
// It redraws itself whenever either endpoint moves.
// ---------------------------------------------------------------
class SeerParallelStacksLiveEdge : public QGraphicsItem {

    public:
        SeerParallelStacksLiveEdge(SeerParallelStacksStackBoxItem* from, SeerParallelStacksStackBoxItem* to, QGraphicsItem* parent = nullptr);
        ~SeerParallelStacksLiveEdge() override;

        QRectF boundingRect     () const override;
        void   paint            (QPainter* painter, const QStyleOptionGraphicsItem* option, QWidget* widget) override;

        // Called by StackBoxItem's destructor so this edge stops referencing
        // an endpoint that is about to be (or has been) destroyed. After this,
        // the edge renders nothing and its own destructor won't touch `box`.
        void   detachEndpoint   (SeerParallelStacksStackBoxItem* box);


    private:
        SeerParallelStacksStackBoxItem*           _from;   // child  (bottom anchor)
        SeerParallelStacksStackBoxItem*           _to;     // parent (top anchor)

        static constexpr qreal  kArrow = 8.0;
        static constexpr qreal  kVCtrl = 60.0 * 0.4;   // bezier control-point stretch
};


class SeerParallelStacksGraphicsView;

// ---------------------------------------------------------------
// Small overlay widget showing the entire scene at a glance, with a
// rectangle marking the main view's current visible region. Click or
// drag inside it to jump/pan the main view there.
// ---------------------------------------------------------------
class SeerParallelStacksMiniMapWidget : public QWidget {

    Q_OBJECT

    public:
        explicit SeerParallelStacksMiniMapWidget(SeerParallelStacksGraphicsView* view, QWidget* parent = nullptr);

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

// A frameless popup window that wraps a QTableWidget inside a small
// bordered frame, and closes/deletes itself when the mouse leaves it.
class SeerParallelStacksPopupTableWidget : public QFrame {

    Q_OBJECT

    public:
        explicit SeerParallelStacksPopupTableWidget(QWidget* parent = nullptr);

    protected:
        void            enterEvent                      (QEnterEvent* event) override;
        void            leaveEvent                      (QEvent* event) override;

    protected slots:
        void            handleCloseTimer                ();

    signals:
        void            mouseLeftPopup                  ();

    private:
        QTableWidget*   _table;
        QTimer*         _closeTimer;
};

class SeerParallelStacksGraphicsView : public QGraphicsView {

    Q_OBJECT

    public:
        explicit SeerParallelStacksGraphicsView(QWidget* parent = nullptr);

        void            setStack                        (const std::shared_ptr<SeerParallelStacksStack>& root);

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
            std::shared_ptr<SeerParallelStacksStack>   stack;
            SeerParallelStacksStackBoxItem*            item   = nullptr;
            PlacedNode*                         parent = nullptr;
            QVector<PlacedNode*>                children;
            qreal                               cx     = 0;
            qreal                               cy     = 0;
        };

        void            buildPlacedTree                 (PlacedNode* pn, const std::shared_ptr<SeerParallelStacksStack>& stack, PlacedNode* parentPN);
        void            layoutTree                      (PlacedNode* pn, qreal& xCursor, qreal yTop);
        void            collectMaxBottom                (PlacedNode* pn, qreal& maxBottom);
        void            alignParentlessToBottom         (PlacedNode* pn, qreal maxBottom);
        void            addEdges                        (PlacedNode* pn);
        void            deleteTree                      (PlacedNode* pn);
        void            repositionMiniMap               (); // keeps it pinned to bottom-right corner

        QGraphicsScene*                   _scene;
        SeerParallelStacksMiniMapWidget*  _miniMap;
        bool                              _panning = false;
        QPoint                            _panStartPos;  // viewport coords at pan start

        friend class SeerParallelStacksMiniMapWidget;    // needs sceneRect()/mapToScene()/centerOn() access
};

