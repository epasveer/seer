#pragma once

#include "SeerCppSourceHighlighter.h"
#include "SeerKeySettings.h"
#include "SeerPlainTextEdit.h"
#include <QtWidgets/QShortcut>
#include <QtWidgets/QWidget>
#include <QtGui/QPaintEvent>
#include <QtGui/QResizeEvent>
#include <QtGui/QPixmap>
#include <QtGui/QTextCursor>
#include <QtCore/QSize>
#include <QtCore/QRect>
#include <QtCore/QString>
#include <QtCore/QStringList>
#include <QtCore/QVector>
#include <QtCore/QMap>

class SeerEditorWidgetAssemblyLineNumberArea;
class SeerEditorWidgetAssemblyBreakPointArea;
class SeerEditorWidgetAssemblyMiniMapArea;

class SeerEditorWidgetAssemblyArea : public SeerPlainTextEdit {

    Q_OBJECT

    public:
        SeerEditorWidgetAssemblyArea (QWidget* parent = 0);

        void                                        enableLineNumberArea                (bool flag);
        bool                                        lineNumberAreaEnabled               () const;

        void                                        enableBreakPointArea                (bool flag);
        bool                                        breakPointAreaEnabled               () const;

        void                                        enableMiniMapArea                   (bool flag);
        bool                                        miniMapAreaEnabled                  () const;

        void                                        lineNumberAreaPaintEvent            (QPaintEvent* event);
        int                                         lineNumberAreaWidth                 ();

        void                                        breakPointAreaPaintEvent            (QPaintEvent* event);
        int                                         breakPointAreaWidth                 ();

        void                                        miniMapAreaPaintEvent               (QPaintEvent* event);
        int                                         miniMapAreaWidth                    ();

        void                                        setAddress                          (const QString& address, bool force=false);
        const QString&                              address                             () const;
        bool                                        setCurrentLine                      (const QString& address);
        void                                        scrollToLine                        (const QString& address);

        int                                         findText                            (const QString& text, QTextDocument::FindFlags flags);
        void                                        clearFindText                       ();

        void                                        clearBreakpoints                    ();
        void                                        addBreakpoint                       (int number, const QString& address, bool enabled);
        bool                                        hasBreakpointNumber                 (int number) const;
        bool                                        hasBreakpointAddress                (const QString& address) const;
        const QVector<int>&                         breakpointNumbers                   () const;
        const QVector<QString>&                     breakpointAddresses                 () const;
        const QVector<bool>&                        breakpointEnableds                  () const;
        int                                         breakpointAddressToNumber           (const QString& address) const;
        bool                                        breakpointAddressEnabled            (const QString& address) const;

        void                                        showContextMenu                     (QMouseEvent* event);
        void                                        showContextMenu                     (QContextMenuEvent* event);
        void                                        showContextMenu                     (const QPoint& pos, const QPoint& globalPos);
        void                                        setQuickBreakpoint                  (QMouseEvent* event);

        void                                        setHighlighterSettings              (const SeerHighlighterSettings& settings);
        const SeerHighlighterSettings&              highlighterSettings                 () const;
        void                                        setHighlighterEnabled               (bool flag);
        bool                                        highlighterEnabled                  () const;

    signals:
        void                                        insertBreakpoint                    (QString breakpoint);
        void                                        insertPrintpoint                    (QString printpoint);
        void                                        deleteBreakpoints                   (QString breakpoints);
        void                                        enableBreakpoints                   (QString breakpoints);
        void                                        disableBreakpoints                  (QString breakpoints);
        void                                        runToAddress                        (QString address);
        void                                        addMemoryVisualize                  (QString expression);
        void                                        addArrayVisualize                   (QString expression);
        void                                        requestAssembly                     (QString address);
        void                                        showSearchBar                       (bool flag);
        void                                        highlighterSettingsChanged          ();

    public slots:
        void                                        handleText                          (const QString& text);
        void                                        handleHighlighterSettingsChanged    ();

    protected:
        void                                        resizeEvent                         (QResizeEvent* event);
        void                                        contextMenuEvent                    (QContextMenuEvent* event);

    private slots:
        void                                        refreshExtraSelections              ();

        void                                        updateMarginAreasWidth              (int newBlockCount);
        void                                        updateLineNumberArea                (const QRect& rect, int dy);
        void                                        updateBreakPointArea                (const QRect& rect, int dy);
        void                                        updateMiniMapArea                   (const QRect& rect, int dy);

    private:
        bool                                        _enableLineNumberArea;
        bool                                        _enableBreakPointArea;
        bool                                        _enableMiniMapArea;
        QVector<int>                                _breakpointsNumbers;
        QVector<QString>                            _breakpointsAddresses;
        QVector<bool>                               _breakpointsEnableds;
        QList<QTextEdit::ExtraSelection>            _findExtraSelections;
        QList<QTextEdit::ExtraSelection>            _currentLinesExtraSelections;

        SeerEditorWidgetAssemblyLineNumberArea*     _lineNumberArea;
        SeerEditorWidgetAssemblyBreakPointArea*     _breakPointArea;
        SeerEditorWidgetAssemblyMiniMapArea*        _miniMapArea;

        QPixmap*                                    _miniMapPixmap;
        SeerHighlighterSettings                     _sourceHighlighterSettings;
        bool                                        _sourceHighlighterEnabled;

        QString                                     _currentAddress;
        QMap<qulonglong,int>                        _addressLineMap;
        QMap<qulonglong,int>                        _offsetLineMap;
        QMap<int,QString>                           _lineAddressMap;
};

class SeerEditorWidgetAssemblyLineNumberArea : public QWidget {

    Q_OBJECT

    public:
        SeerEditorWidgetAssemblyLineNumberArea (SeerEditorWidgetAssemblyArea* editorWidget);

        QSize                                       sizeHint                            () const override;

    protected:
        void                                        paintEvent                          (QPaintEvent* event) override;
        void                                        mouseDoubleClickEvent               (QMouseEvent* event) override;
        void                                        mouseMoveEvent                      (QMouseEvent* event) override;
        void                                        mousePressEvent                     (QMouseEvent* event) override;
        void                                        mouseReleaseEvent                   (QMouseEvent* event) override;

    private:
        SeerEditorWidgetAssemblyArea*               _editorWidget;
};

class SeerEditorWidgetAssemblyBreakPointArea : public QWidget {

    Q_OBJECT

    public:
        SeerEditorWidgetAssemblyBreakPointArea (SeerEditorWidgetAssemblyArea* editorWidget);

        QSize                                       sizeHint                            () const override;

    protected:
        void                                        paintEvent                          (QPaintEvent* event) override;
        void                                        mouseDoubleClickEvent               (QMouseEvent* event) override;
        void                                        mouseMoveEvent                      (QMouseEvent* event) override;
        void                                        mousePressEvent                     (QMouseEvent* event) override;
        void                                        mouseReleaseEvent                   (QMouseEvent* event) override;

    private:
        SeerEditorWidgetAssemblyArea*               _editorWidget;
};

class SeerEditorWidgetAssemblyMiniMapArea : public QWidget {

    Q_OBJECT

    public:
        SeerEditorWidgetAssemblyMiniMapArea (SeerEditorWidgetAssemblyArea* editorWidget);

        QSize                                       sizeHint                            () const override;

    protected:
        void                                        paintEvent                          (QPaintEvent* event) override;
        void                                        mouseDoubleClickEvent               (QMouseEvent* event) override;
        void                                        mouseMoveEvent                      (QMouseEvent* event) override;
        void                                        mousePressEvent                     (QMouseEvent* event) override;
        void                                        mouseReleaseEvent                   (QMouseEvent* event) override;

    private:
        SeerEditorWidgetAssemblyArea*               _editorWidget;
};

#include "ui_SeerEditorWidgetAssembly.h"

class SeerEditorWidgetAssembly : public QWidget, protected Ui::SeerEditorWidgetAssemblyForm {

    Q_OBJECT

    public:
        explicit SeerEditorWidgetAssembly (QWidget* parent = 0);
       ~SeerEditorWidgetAssembly ();

        SeerEditorWidgetAssemblyArea*               assemblyArea                        ();

        bool                                        isSearchBarShown                    () const;
        bool                                        searchMatchCase                     () const;
        void                                        setKeySettings                      (const SeerKeySettings& settings);
        const SeerKeySettings&                      keySettings                         () const;

    public slots:
        void                                        reloadAssembly                      ();
        void                                        showSearchBar                       (bool flag);
        void                                        setSearchMatchCase                  (bool flag);

    private slots:
        void                                        handleSearchLineNumberLineEdit      ();
        void                                        handleSearchTextLineEdit            ();
        void                                        handleSearchDownToolButton          ();
        void                                        handleSearchUpToolButton            ();
        void                                        handleSearchCloseToolButton         ();
        void                                        handleTextSearchShortcut            ();

    signals:
    private:
        SeerKeySettings                             _keySettings;
        QShortcut*                                  _textSearchShortcut;
        QShortcut*                                  _textSearchNextShortcut;
        QShortcut*                                  _textSearchPrevShortcut;
};

