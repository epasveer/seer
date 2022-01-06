#pragma once

#include "SeerCppSourceHighlighter.h"
#include <QtWidgets/QWidget>
#include <QtWidgets/QPlainTextEdit>
#include <QtGui/QPaintEvent>
#include <QtGui/QResizeEvent>
#include <QtGui/QPixmap>
#include <QtGui/QTextCursor>
#include <QtCore/QSize>
#include <QtCore/QRect>
#include <QtCore/QString>
#include <QtCore/QVector>

class SeerEditorWidgetSourceArea : public QPlainTextEdit {

    Q_OBJECT

    public:
        SeerEditorWidgetSourceArea (QWidget* parent = 0);

        void                            enableLineNumberArea            (bool flag);
        bool                            lineNumberAreaEnabled           () const;

        void                            enableBreakPointArea            (bool flag);
        bool                            breakPointAreaEnabled           () const;

        void                            enableMiniMapArea               (bool flag);
        bool                            miniMapAreaEnabled              () const;

        void                            lineNumberAreaPaintEvent        (QPaintEvent* event);
        int                             lineNumberAreaWidth             ();

        void                            breakPointAreaPaintEvent        (QPaintEvent* event);
        int                             breakPointAreaWidth             ();

        void                            miniMapAreaPaintEvent           (QPaintEvent* event);
        int                             miniMapAreaWidth                ();

        bool                            isOpen                          () const;
        void                            open                            (const QString& fullname, const QString& file);
        void                            openText                        (const QString& text,     const QString& file);
        const QString&                  fullname                        () const;
        const QString&                  file                            () const;
        void                            close                           ();
        void                            setCurrentLine                  (int lineno);
        void                            scrollToLine                    (int lineno);

        void                            clearCurrentLines               ();
        void                            addCurrentLine                  (int lineno);

        void                            clearBreakpoints                ();
        void                            addBreakpoint                   (int number, int lineno, bool enabled);
        bool                            hasBreakpointNumber             (int number) const;
        bool                            hasBreakpointLine               (int lineno) const;
        const QVector<int>&             breakpointNumbers               () const;
        const QVector<int>&             breakpointLines                 () const;
        const QVector<bool>&            breakpointEnableds              () const;
        int                             breakpointLineToNumber          (int lineno) const;
        bool                            breakpointLineEnabled           (int lineno) const;

        void                            showContextMenu                 (QMouseEvent* event);
        void                            showContextMenu                 (QContextMenuEvent* event);
        void                            showContextMenu                 (const QPoint& pos, const QPoint& globalPos);
        void                            setQuickBreakpoint              (QMouseEvent* event);

        void                            clearExpression                 ();

        void                            setHighlighterSettings          (const SeerHighlighterSettings& settings);
        const SeerHighlighterSettings&  highlighterSettings             () const;

    signals:
        void                            insertBreakpoint                (QString breakpoint);
        void                            deleteBreakpoints               (QString breakpoints);
        void                            enableBreakpoints               (QString breakpoints);
        void                            disableBreakpoints              (QString breakpoints);
        void                            runToLine                       (QString fullname, int lineno);
        void                            addVariableLoggerExpression     (QString expression);
        void                            addVariableTrackerExpression    (QString expression);
        void                            refreshVariableTrackerValues    ();
        void                            evaluateVariableExpression      (int expressionid, QString expression);
        void                            addMemoryVisualize              (QString expression);
        void                            showSearchBar                   (bool flag);
        void                            showAlternateBar                (bool flag);

    public slots:
        void                            handleText                      (const QString& text);

    protected:
        void                            resizeEvent                     (QResizeEvent* event);
        void                            contextMenuEvent                (QContextMenuEvent* event);
        void                            keyPressEvent                   (QKeyEvent* event);
        void                            mouseReleaseEvent               (QMouseEvent* event);
        bool                            event                           (QEvent* event);

    private slots:
        void                            highlightCurrentLines           ();
        void                            highlightCurrentLine            ();
        void                            unhighlightCurrentLine          ();

        void                            updateMarginAreasWidth          (int newBlockCount);
        void                            updateLineNumberArea            (const QRect& rect, int dy);
        void                            updateBreakPointArea            (const QRect& rect, int dy);
        void                            updateMiniMapArea               (const QRect& rect, int dy);

    private:
        QString                         _fullname;
        QString                         _file;
        bool                            _enableLineNumberArea;
        bool                            _enableBreakPointArea;
        bool                            _enableMiniMapArea;
        QVector<int>                    _breakpointsNumbers;
        QVector<int>                    _breakpointsLineNumbers;
        QVector<bool>                   _breakpointsEnableds;
        QVector<QTextCursor>            _currentLineCursors;

        QTextCursor                     _selectedExpressionCursor;
        int                             _selectedExpressionId;
        QString                         _selectedExpressionName;
        QString                         _selectedExpressionValue;
        bool                            _ctrlKeyPressed;

        QWidget*                        _lineNumberArea;
        QWidget*                        _breakPointArea;
        QWidget*                        _miniMapArea;

        QPixmap*                        _miniMapPixmap;
        SeerCppSourceHighlighter*       _sourceHighlighter;
        SeerHighlighterSettings         _sourceHighlighterSettings;
};

class SeerEditorWidgetLineNumberArea : public QWidget {
    public:
        SeerEditorWidgetLineNumberArea (SeerEditorWidgetSourceArea* editorWidget);

        QSize                           sizeHint                    () const override;

    protected:
        void                            paintEvent                  (QPaintEvent* event) override;
        void                            mouseDoubleClickEvent       (QMouseEvent* event) override;
        void                            mouseMoveEvent              (QMouseEvent* event) override;
        void                            mousePressEvent             (QMouseEvent* event) override;
        void                            mouseReleaseEvent           (QMouseEvent* event) override;


    private:
        SeerEditorWidgetSourceArea*     _editorWidget;
};

class SeerEditorWidgetBreakPointArea : public QWidget {
    public:
        SeerEditorWidgetBreakPointArea (SeerEditorWidgetSourceArea* editorWidget);

        QSize                           sizeHint                    () const override;

    protected:
        void                            paintEvent                  (QPaintEvent* event) override;
        void                            mouseDoubleClickEvent       (QMouseEvent* event) override;
        void                            mouseMoveEvent              (QMouseEvent* event) override;
        void                            mousePressEvent             (QMouseEvent* event) override;
        void                            mouseReleaseEvent           (QMouseEvent* event) override;


    private:
        SeerEditorWidgetSourceArea*     _editorWidget;
};

class SeerEditorWidgetMiniMapArea : public QWidget {
    public:
        SeerEditorWidgetMiniMapArea (SeerEditorWidgetSourceArea* editorWidget);

        QSize                           sizeHint                    () const override;

    protected:
        void                            paintEvent                  (QPaintEvent* event) override;
        void                            mouseDoubleClickEvent       (QMouseEvent* event) override;
        void                            mouseMoveEvent              (QMouseEvent* event) override;
        void                            mousePressEvent             (QMouseEvent* event) override;
        void                            mouseReleaseEvent           (QMouseEvent* event) override;


    private:
        SeerEditorWidgetSourceArea*     _editorWidget;
};

#include "ui_SeerEditorWidget.h"

class SeerEditorWidget : public QWidget, protected Ui::SeerEditorWidgetForm {

    Q_OBJECT

    public:
        explicit SeerEditorWidget (QWidget* parent = 0);
       ~SeerEditorWidget ();

        SeerEditorWidgetSourceArea*     sourceArea                  ();

        void                            handleLineNumberLineEdit    ();
        void                            handleSearchTextLineEdit    ();
        void                            handleSearchDownToolButton  ();
        void                            handleSearchUpToolButton    ();
        void                            handleCloseToolButton       ();

    public slots:
        void                            showSearchBar               (bool flag);
        bool                            isSearchBarShown            () const;
        void                            showAlternateBar            (bool flag);
        bool                            isAlternateBarShown         () const;
};

