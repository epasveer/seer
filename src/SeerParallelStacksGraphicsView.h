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
#include <QStringList>

class SeerParallelStacksPopupTableWidget;

class SeerParallelStacksLiveEdge;

// ---------------------------------------------------------------
// A single box in the graph: shows thread count + IDs + call frames.
// Ctrl+LMB grabs and moves the item freely within the scene.
// Moving the item automatically redraws all connected LiveEdges.
// ---------------------------------------------------------------
class SeerParallelStacksStackBoxItem : public QObject, public QGraphicsItem {

    Q_OBJECT
    Q_INTERFACES(QGraphicsItem)

    public:
        explicit SeerParallelStacksStackBoxItem(const SeerParallelStacksStack& stack, const SeerParallelStacksSettings& settings, QGraphicsItem* parent = nullptr);
       ~SeerParallelStacksStackBoxItem() override;

        QRectF                  boundingRect            () const override;
        void                    paint                   (QPainter* painter, const QStyleOptionGraphicsItem* option, QWidget* widget) override;

        qreal                   width                   () const;
        qreal                   height                  () const;

        // Edge registry — called by LiveEdge on construction/destruction
        void                    registerEdge            (SeerParallelStacksLiveEdge* e);
        void                    unregisterEdge          (SeerParallelStacksLiveEdge* e);

        // Bottom-centre and top-centre in scene coordinates (edge attach points)
        QPointF                 sceneBottom             () const;
        QPointF                 sceneTop                () const;

    protected:
        QVariant                itemChange              (GraphicsItemChange change, const QVariant& value) override;

        void                    mousePressEvent         (QGraphicsSceneMouseEvent* event) override;
        void                    mouseMoveEvent          (QGraphicsSceneMouseEvent* event) override;
        void                    mouseReleaseEvent       (QGraphicsSceneMouseEvent* event) override;
        void                    hoverEnterEvent         (QGraphicsSceneHoverEvent* event) override;
        void                    hoverLeaveEvent         (QGraphicsSceneHoverEvent* event) override;

    private slots:
        void                    handleDeletePopup       ();
        void                    handleShowPopup         ();
        void                    handleMaybeClosePopup   ();

    private:
        // Global (screen) rect this box occupies, used to test whether the
        // cursor is still over the node.
        QRect                   globalRect              () const;

        // Build the list of frame-row texts to draw. Honors the
        // showFullStackSize / stackSize settings: when the full stack is
        // hidden, only the top and bottom stackSize frames are shown and the
        // removed middle ones are replaced by a single "[...]" row. The
        // underlying _stack.frames is left untouched.
        QStringList             buildFrameRows          () const;

        QVector<SeerParallelStacksLiveEdge*>            _edges;   // non-owning
        QVector<int>                                    _threadIds;
        SeerParallelStacksStack                         _stack;
        QStringList                                     _frameRows;
        SeerParallelStacksSettings                      _settings;
        QString                                         _headerLeft;
        QString                                         _headerRight;
        qreal                                           _width          = 0;
        qreal                                           _height         = 0;
        SeerParallelStacksPopupTableWidget*             _popup          = 0;
        QTimer*                                         _hoverTimer     = 0;
        static constexpr int                            _kHoverDelayMs  = 1000;
        static constexpr int                            _kCloseGraceMs  = 150;
        bool                                            _dragging       = false;
        QPointF                                         _dragOffset;
        static constexpr qreal                          _kPadX          = 12;
        static constexpr qreal                          _kPadY          =  8;
        static constexpr qreal                          _kRowH          = 20;
        static constexpr qreal                          _kHeaderGap     = 16;
};

// ---------------------------------------------------------------
// A live bezier edge between two StackBoxItems.
// It redraws itself whenever either endpoint moves.
// ---------------------------------------------------------------
class SeerParallelStacksLiveEdge : public QGraphicsItem {

    public:
        SeerParallelStacksLiveEdge(SeerParallelStacksStackBoxItem* from, SeerParallelStacksStackBoxItem* to, QGraphicsItem* parent = nullptr);
       ~SeerParallelStacksLiveEdge() override;

        QRectF          boundingRect     () const override;
        QPainterPath    shape            () const override;
        void            paint            (QPainter* painter, const QStyleOptionGraphicsItem* option, QWidget* widget) override;

        // Called by StackBoxItem's destructor so this edge stops referencing
        // an endpoint that is about to be (or has been) destroyed. After this,
        // the edge renders nothing and its own destructor won't touch `box`.
        void            detachEndpoint   (SeerParallelStacksStackBoxItem* box);


    private:
        SeerParallelStacksStackBoxItem*     _from;   // child  (bottom anchor)
        SeerParallelStacksStackBoxItem*     _to;     // parent (top anchor)

        static constexpr qreal              _kArrow = 8.0;
        static constexpr qreal              _kVCtrl = 60.0 * 0.4;   // bezier control-point stretch
};


class SeerParallelStacksGraphicsView;

// ---------------------------------------------------------------
// Small overlay widget showing the entire scene at a glance, with a
// rectangle marking the main view's current visible region. Click or
// drag inside it to jump/pan the main view there. Shift + left-drag
// moves the minimap widget itself to a new spot in the view.
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

        // The scene region the overview frames: the graph's nodes plus the
        // current visible region (so the viewport marker is always shown),
        // independent of where the minimap itself has been parked.
        QRectF  overviewSceneRect       () const;

        SeerParallelStacksGraphicsView* _view;
        bool                            _dragging = false;   // plain left-drag: navigating the graph
        bool                            _moving   = false;   // shift + left-drag: repositioning this widget
        QPoint                          _moveGrabOffset;     // cursor offset within the widget at move start
};

// A frameless popup window that wraps a QTableWidget inside a small
// bordered frame. Its lifetime is owned by the SeerParallelStacksStackBoxItem
// that created it — it stays up for as long as the mouse is in that node.
class SeerParallelStacksPopupTableWidget : public QFrame {

    Q_OBJECT

    public:
        explicit SeerParallelStacksPopupTableWidget(QWidget* parent = nullptr);

        void            addRow                          (int threadid, const QString& function);

    protected:
        void            leaveEvent                      (QEvent* event) override;

    signals:
        void            mouseLeftPopup                  ();

    private:
        QTableWidget*   _table;
};

class SeerParallelStacksGraphicsView : public QGraphicsView {

    Q_OBJECT

    public:
        explicit SeerParallelStacksGraphicsView(QWidget* parent = nullptr);

        void            setStack                        (const SeerParallelStacksStack& root, const SeerParallelStacksSettings& settings);
        void            setColorTheme                   (const QString& colorTheme);
        void            setShowMinimapMode              (const QString& mode);   // "Always", "Never", or "Auto"

        // Drag-autoscroll hooks. While a node or the minimap is being dragged
        // near a viewport edge, the view scrolls (and grows the scene) to keep
        // the dragged object under the cursor. Driven by the node item and the
        // minimap widget.
        void            beginNodeDragScroll             ();
        void            beginMiniMapDragScroll          ();
        void            updateDragScroll                (const QPoint& viewportPos);
        void            endDragScroll                   ();

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

        // Fires while a drag sits near a viewport edge: nudges the scrollbars
        // (and the dragged object) so it keeps following the cursor.
        void            handleAutoScrollTick            ();

    private:
        struct PlacedNode {
            SeerParallelStacksStack                 stack;
            SeerParallelStacksStackBoxItem*         item   = nullptr;
            PlacedNode*                             parent = nullptr;
            QVector<PlacedNode*>                    children;
            qreal                                   cx     = 0;
            qreal                                   cy     = 0;
        };

        void            buildPlacedTree             (PlacedNode* pn, const SeerParallelStacksStack& stack, const SeerParallelStacksSettings& settings, PlacedNode* parentPN);
        void            layoutTree                  (PlacedNode* pn, qreal& xCursor, qreal yTop);
        void            collectMaxBottom            (PlacedNode* pn, qreal& maxBottom);
        void            alignParentlessToBottom     (PlacedNode* pn, qreal maxBottom);
        void            addEdges                    (PlacedNode* pn);
        void            deleteTree                  (PlacedNode* pn);

        // Zoom: '+'/'-' step (keyboard, centered), mouse wheel (under cursor).
        void            zoomBy                      (double factor, QGraphicsView::ViewportAnchor anchor);
        // 'Esc' — refit the whole graph to the viewport (normal zoom level).
        void            resetZoom                   ();

        // Places the minimap widget. Until the user Shift-drags it, this pins
        // it to the bottom-right corner of the viewport. Once dragged, the
        // minimap is anchored to a scene position and travels with the graph —
        // like a node — and this re-derives its widget geometry.
        void            repositionMiniMap           ();

        // Anchors the minimap at the scene point currently under viewTopLeft
        // (view coordinates). Called by the minimap widget while it is being
        // Shift-dragged.
        void            placeMiniMapAt              (const QPoint& viewTopLeft);

        // Scene rectangle the minimap widget currently covers.
        QRectF          miniMapSceneRect            () const;

        // Grows the scene rect so it always contains the (Shift-dragged)
        // minimap plus padding — the "make room for it" behavior nodes get
        // from handleGrowSceneRectToFitItems().
        void            growSceneForMiniMap         ();

        // Shows or hides the minimap according to _showMinimapMode. In "Auto"
        // mode the minimap is shown only while one of the view's scrollbars is
        // active (i.e. the scene doesn't fully fit in the viewport).
        void            updateMiniMapVisibility     ();

        QGraphicsScene*                             _scene;
        SeerParallelStacksMiniMapWidget*            _miniMap;
        QString                                     _showMinimapMode = "Auto";
        bool                                        _miniMapAnchored = false;   // true once the user has Shift-dragged it
        QPointF                                     _miniMapAnchor;             // scene coords of the minimap's top-left
        bool                                        _panning = false;
        QPoint                                      _panStartPos;  // viewport coords at pan start

        QTimer                                      _autoScrollTimer;
        QPoint                                      _autoScrollVelocity;        // px/tick, from cursor proximity to the edges
        bool                                        _nodeDragScroll    = false;
        bool                                        _miniMapDragScroll = false;

        friend class SeerParallelStacksMiniMapWidget;    // needs sceneRect()/mapToScene()/centerOn() access
};

