#pragma once

#include "SeerCppSourceHighlighter.h"
#include "SeerKeySettings.h"
#include "SeerPlainTextEdit.h"
#include <QtGui/QShortcut>
#include <QtGui/QPaintEvent>
#include <QtGui/QResizeEvent>
#include <QtGui/QPixmap>
#include <QtGui/QTextCursor>
#include <QtWidgets/QWidget>
#include <QtWidgets/QMenu>
#include <QtCore/QSize>
#include <QtCore/QRect>
#include <QtCore/QString>
#include <QtCore/QStringList>
#include <QtCore/QVector>
#include <QtCore/QMap>

class SeerEditorWidgetAssemblyLineNumberArea;
class SeerEditorWidgetAssemblyOffsetArea;
class SeerEditorWidgetAssemblyBreakPointArea;
class SeerEditorWidgetAssemblyOpcodeArea;

class SeerEditorWidgetAssemblyArea : public SeerPlainTextEdit {

    Q_OBJECT

    public:
        SeerEditorWidgetAssemblyArea (QWidget* parent = 0);

        void                                        enableLineNumberArea                (bool flag);
        bool                                        lineNumberAreaEnabled               () const;

        void                                        enableOffsetArea                    (bool flag);
        bool                                        offsetAreaEnabled                   () const;

        void                                        enableBreakPointArea                (bool flag);
        bool                                        breakPointAreaEnabled               () const;

        void                                        enableOpcodeArea                    (bool flag);
        bool                                        opcodeAreaEnabled                   () const;

        void                                        enableSourceLines                   (bool flag);
        bool                                        sourceLinesEnabled                  () const;

        void                                        lineNumberAreaPaintEvent            (QPaintEvent* event);
        int                                         lineNumberAreaWidth                 ();

        void                                        offsetAreaPaintEvent                (QPaintEvent* event);
        int                                         offsetAreaWidth                     ();

        void                                        breakPointAreaPaintEvent            (QPaintEvent* event);
        int                                         breakPointAreaWidth                 ();

        void                                        opcodeAreaPaintEvent                (QPaintEvent* event);
        int                                         opcodeAreaWidth                     ();

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
        void                                        showContextMenu                     (const QPoint& pos, const QPointF& globalPos);
        void                                        setQuickBreakpoint                  (QMouseEvent* event);
        void                                        setQuickRunToAddress                (QMouseEvent* event);

        void                                        setHighlighterSettings              (const SeerHighlighterSettings& settings);
        const SeerHighlighterSettings&              highlighterSettings                 () const;
        void                                        setHighlighterEnabled               (bool flag);
        bool                                        highlighterEnabled                  () const;

        QString                                     sourceForLine                       (const QString& fullname, const QString& file, int line);

        void                                        setEditorFont                       (const QFont& font);
        const QFont&                                editorFont                          () const;
        void                                        setEditorTabSize                    (int spaces);
        int                                         editorTabSize                       () const;

    signals:
        void                                        insertBreakpoint                    (QString breakpoint);
        void                                        insertPrintpoint                    (QString printpoint);
        void                                        deleteBreakpoints                   (QString breakpoints);
        void                                        enableBreakpoints                   (QString breakpoints);
        void                                        disableBreakpoints                  (QString breakpoints);
        void                                        refreshBreakpointsStackFrames       ();
        void                                        runToAddress                        (QString address);
        void                                        addMemoryVisualize                  (QString expression);
        void                                        addArrayVisualize                   (QString expression);
        void                                        addStructVisualize                  (QString expression);
        void                                        requestAssembly                     (QString address);
        void                                        requestSourceAndAssembly            (QString address);
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

        void                                        updateTextArea                      ();
        void                                        updateMarginAreasWidth              (int newBlockCount);
        void                                        updateLineNumberArea                (const QRect& rect, int dy);
        void                                        updateOffsetArea                    (const QRect& rect, int dy);
        void                                        updateBreakPointArea                (const QRect& rect, int dy);
        void                                        updateOpcodeArea                    (const QRect& rect, int dy);

    private:
        bool                                        _enableLineNumberArea;
        bool                                        _enableOffsetArea;
        bool                                        _enableBreakPointArea;
        bool                                        _enableOpcodeArea;
        bool                                        _enableSourceLines;
        QVector<int>                                _breakpointsNumbers;
        QVector<QString>                            _breakpointsAddresses;
        QVector<bool>                               _breakpointsEnableds;
        QList<QTextEdit::ExtraSelection>            _findExtraSelections;
        QList<QTextEdit::ExtraSelection>            _currentLinesExtraSelections;
        QList<QTextEdit::ExtraSelection>            _sourceLinesExtraSelections;

        SeerEditorWidgetAssemblyLineNumberArea*     _lineNumberArea;
        SeerEditorWidgetAssemblyOffsetArea*         _offsetArea;
        SeerEditorWidgetAssemblyBreakPointArea*     _breakPointArea;
        SeerEditorWidgetAssemblyOpcodeArea*         _opcodeArea;

        SeerHighlighterSettings                     _sourceHighlighterSettings;
        bool                                        _sourceHighlighterEnabled;

        QString                                     _currentAddress;
        QMap<qulonglong,int>                        _addressLineMap;
        QMap<qulonglong,int>                        _offsetLineMap;
        QMap<int,QString>                           _lineAddressMap;
        QMap<int,qulonglong>                        _lineOffsetMap;
        QMap<int,QString>                           _lineOpcodeMap;

        // Text from asm_insns command.
        QString                                     _asm_insns_text;

        // Source lines for assembly file.
        QStringList                                 _fileLines;
        QString                                     _fileFullname;
        QString                                     _fileName;

        int                                         _sourceTabSize;
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

class SeerEditorWidgetAssemblyOffsetArea : public QWidget {

    Q_OBJECT

    public:
        SeerEditorWidgetAssemblyOffsetArea (SeerEditorWidgetAssemblyArea* editorWidget);

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

class SeerEditorWidgetAssemblyOpcodeArea : public QWidget {

    Q_OBJECT

    public:
        SeerEditorWidgetAssemblyOpcodeArea (SeerEditorWidgetAssemblyArea* editorWidget);

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
        bool                                        showAddressColumn                   () const;
        bool                                        showOffsetColumn                    () const;
        bool                                        showOpcodeColumn                    () const;
        bool                                        showSourceLines                     () const;
        QString                                     regiserNamePC                       () const;
        QString                                     regiserNameFLAGS                    () const;
        QString                                     regiserNameSP                       () const;

        void                                        setKeySettings                      (const SeerKeySettings& settings);
        const SeerKeySettings&                      keySettings                         () const;

    public slots:
        void                                        reloadAssembly                      ();
        void                                        reloadRegisters                     ();
        void                                        showSearchBar                       (bool flag);
        void                                        setSearchMatchCase                  (bool flag);
        void                                        setShowAddressColumn                (bool flag);
        void                                        setShowOffsetColumn                 (bool flag);
        void                                        setShowOpcodeColumn                 (bool flag);
        void                                        setShowSourceLines                  (bool flag);
        void                                        setRegiserNamePC                    (const QString& name);
        void                                        setRegiserNameFLAGS                 (const QString& name);
        void                                        setRegiserNameSP                    (const QString& name);
        void                                        handleText                          (const QString& text);

    private slots:
        void                                        handleSearchLineNumberLineEdit      ();
        void                                        handleSearchTextLineEdit            ();
        void                                        handleSearchDownToolButton          ();
        void                                        handleSearchUpToolButton            ();
        void                                        handleSearchCloseToolButton         ();
        void                                        handleTextSearchShortcut            ();
        void                                        handleShowAddressColumn             ();
        void                                        handleShowOffsetColumn              ();
        void                                        handleShowOpcodeColumn              ();
        void                                        handleShowSourceLines               ();
        void                                        handleEditPreferences               ();

    signals:
        void                                        evaluateVariableExpression          (int expressionid, QString expression);

    protected:
        void                                        writeSettings                       ();
        void                                        readSettings                        ();

    private:
        int                                         _pcId;
        int                                         _spId;
        int                                         _flagsId;
        SeerKeySettings                             _keySettings;
        QShortcut*                                  _textSearchShortcut;
        QShortcut*                                  _textSearchNextShortcut;
        QShortcut*                                  _textSearchPrevShortcut;
        QAction*                                    _editPreferencesAction;
        QString                                     _pcName;
        QString                                     _spName;
        QString                                     _flagsName;
};

