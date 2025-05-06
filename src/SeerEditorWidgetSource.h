#pragma once

#include "SeerCppSourceHighlighter.h"
#include "SeerKeySettings.h"
#include "SeerPlainTextEdit.h"
#include <QShortcut>
#include <QtWidgets/QWidget>
#include <QtGui/QPaintEvent>
#include <QtGui/QResizeEvent>
#include <QtGui/QTextCursor>
#include <QtCore/QSize>
#include <QtCore/QRect>
#include <QtCore/QString>
#include <QtCore/QStringList>
#include <QtCore/QVector>
#include <QtCore/QMap>
#include <QtCore/QFileSystemWatcher>
#include <QtCore/QPoint>


class SeerEditorWidgetSourceLineNumberArea;
class SeerEditorWidgetSourceBreakPointArea;

class SeerEditorWidgetSourceArea : public SeerPlainTextEdit {

    Q_OBJECT

    public:
        SeerEditorWidgetSourceArea (QWidget* parent = 0);

        void                                        enableLineNumberArea                (bool flag);
        bool                                        lineNumberAreaEnabled               () const;

        void                                        enableBreakPointArea                (bool flag);
        bool                                        breakPointAreaEnabled               () const;

        void                                        lineNumberAreaPaintEvent            (QPaintEvent* event);
        int                                         lineNumberAreaWidth                 ();

        void                                        breakPointAreaPaintEvent            (QPaintEvent* event);
        int                                         breakPointAreaWidth                 ();

        bool                                        isOpen                              () const;
        void                                        open                                (const QString& fullname, const QString& file, const QString& alternateDirectory="");
        void                                        openText                            (const QString& text,     const QString& file);
        void                                        close                               ();
        void                                        reload                              ();
        const QString&                              fullname                            () const;
        const QString&                              file                                () const;
        void                                        setAlternateDirectory               (const QString& alternateDirectory);
        const QString&                              alternateDirectory                  () const;
        void                                        setAlternateDirectories             (const QStringList& alternateDirectories);
        const QStringList&                          alternateDirectories                () const;
        QString                                     findFile                            (const QString& file, const QString& fullname, const QString& alternateDirectory, const QStringList& alternateDirectories);

        void                                        setCurrentLine                      (int lineno);
        void                                        scrollToLine                        (int lineno);

        void                                        clearCurrentLines                   ();
        void                                        addCurrentLine                      (int lineno, int level);
        int                                         findText                            (const QString& text, QTextDocument::FindFlags flags);
        void                                        clearFindText                       ();

        void                                        clearBreakpoints                    ();
        void                                        addBreakpoint                       (int number, int lineno, bool enabled);
        bool                                        hasBreakpointNumber                 (int number) const;
        bool                                        hasBreakpointLine                   (int lineno) const;
        const QVector<int>&                         breakpointNumbers                   () const;
        const QVector<int>&                         breakpointLines                     () const;
        const QVector<bool>&                        breakpointEnableds                  () const;
        int                                         breakpointLineToNumber              (int lineno) const;
        bool                                        breakpointLineEnabled               (int lineno) const;

        void                                        showContextMenu                     (QMouseEvent* event);
        void                                        showContextMenu                     (QContextMenuEvent* event);
        void                                        showContextMenu                     (const QPoint& pos, const QPointF& globalPos);
        void                                        setQuickBreakpoint                  (QMouseEvent* event);
        void                                        setQuickRunToLine                   (QMouseEvent* event);
        void                                        showBreakpointToolTip               (QMouseEvent* event);

        void                                        clearExpression                     ();

        void                                        setHighlighterSettings              (const SeerHighlighterSettings& settings);
        const SeerHighlighterSettings&              highlighterSettings                 () const;
        void                                        setHighlighterEnabled               (bool flag);
        bool                                        highlighterEnabled                  () const;

        void                                        setEditorFont                       (const QFont& font);
        const QFont&                                editorFont                          () const;
        void                                        setEditorTabSize                    (int spaces);
        int                                         editorTabSize                       () const;
        void                                        setExternalEditorCommand            (const QString& externalEditorCommand);
        const QString&                              externalEditorCommand               ();

    signals:
        void                                        insertBreakpoint                    (QString breakpoint);
        void                                        insertPrintpoint                    (QString type, QString function, QString channel, QString parameters);
        void                                        deleteBreakpoints                   (QString breakpoints);
        void                                        enableBreakpoints                   (QString breakpoints);
        void                                        disableBreakpoints                  (QString breakpoints);
        void                                        infoBreakpoint                      (int breakpointid, QString breakpoint);
        void                                        refreshBreakpointsStackFrames       ();
        void                                        runToLine                           (QString fullname, int lineno);
        void                                        addVariableLoggerExpression         (QString expression);
        void                                        addVariableTrackerExpression        (QString expression);
        void                                        refreshVariableTrackerValues        ();
        void                                        evaluateVariableExpression          (int expressionid, QString expression);
        void                                        addMemoryVisualize                  (QString expression);
        void                                        addArrayVisualize                   (QString expression);
        void                                        addStructVisualize                  (QString expression);
        void                                        showSearchBar                       (bool flag);
        void                                        showAlternateBar                    (bool flag);
        void                                        showReloadBar                       (bool flag);
        void                                        highlighterSettingsChanged          ();

    public slots:
        void                                        handleText                          (const QString& text);
        void                                        handleHighlighterSettingsChanged    ();
        void                                        handleWatchFileModified             (const QString& path);
        void                                        handleBreakpointToolTip             (QPoint pos, const QString& text);

    protected:
        void                                        resizeEvent                         (QResizeEvent* event);
        void                                        contextMenuEvent                    (QContextMenuEvent* event);
        void                                        mouseReleaseEvent                   (QMouseEvent* event);
        bool                                        event                               (QEvent* event);
        void                                        showExpressionTooltip               ();
        void                                        hideExpressionTooltip               ();

    private slots:
        void                                        refreshExtraSelections              ();

        void                                        updateMarginAreasWidth              (int newBlockCount);
        void                                        updateLineNumberArea                (const QRect& rect, int dy);
        void                                        updateBreakPointArea                (const QRect& rect, int dy);

    private:
        QString                                     _fullname;
        QString                                     _file;
        QString                                     _alternateDirectory;
        QStringList                                 _alternateDirectories;
        QFileSystemWatcher*                         _fileWatcher;

        bool                                        _enableLineNumberArea;
        bool                                        _enableBreakPointArea;
        QVector<int>                                _breakpointsNumbers;
        QVector<int>                                _breakpointsLineNumbers;
        QVector<bool>                               _breakpointsEnableds;
        QList<QTextEdit::ExtraSelection>            _findExtraSelections;
        QList<QTextEdit::ExtraSelection>            _currentLinesExtraSelections;

        QTextCursor                                 _selectedExpressionCursor;
        QPoint                                      _selectedExpressionPosition;
        int                                         _selectedExpressionId;
        QString                                     _selectedExpressionName;
        QString                                     _selectedExpressionValue;
        QPoint                                      _selectedBreakpointPosition;
        int                                         _selectedBreakpointId;

        SeerEditorWidgetSourceLineNumberArea*       _lineNumberArea;
        SeerEditorWidgetSourceBreakPointArea*       _breakPointArea;

        SeerCppSourceHighlighter*                   _sourceHighlighter;
        SeerHighlighterSettings                     _sourceHighlighterSettings;
        bool                                        _sourceHighlighterEnabled;

        int                                         _sourceTabSize;
        QString                                     _externalEditorCommand;
};

class SeerEditorWidgetSourceLineNumberArea : public QWidget {

    Q_OBJECT

    public:
        SeerEditorWidgetSourceLineNumberArea (SeerEditorWidgetSourceArea* editorWidget);

        QSize                                       sizeHint                            () const override;

    protected:
        void                                        paintEvent                          (QPaintEvent* event) override;
        void                                        mouseDoubleClickEvent               (QMouseEvent* event) override;
        void                                        mouseMoveEvent                      (QMouseEvent* event) override;
        void                                        mousePressEvent                     (QMouseEvent* event) override;
        void                                        mouseReleaseEvent                   (QMouseEvent* event) override;

    private:
        SeerEditorWidgetSourceArea*                 _editorWidget;
};

class SeerEditorWidgetSourceBreakPointArea : public QWidget {

    Q_OBJECT

    public:
        SeerEditorWidgetSourceBreakPointArea (SeerEditorWidgetSourceArea* editorWidget);

        QSize                                       sizeHint                            () const override;

    protected:
        void                                        paintEvent                          (QPaintEvent* event) override;
        void                                        mouseDoubleClickEvent               (QMouseEvent* event) override;
        void                                        mouseMoveEvent                      (QMouseEvent* event) override;
        void                                        mousePressEvent                     (QMouseEvent* event) override;
        void                                        mouseReleaseEvent                   (QMouseEvent* event) override;

    private:
        SeerEditorWidgetSourceArea*                 _editorWidget;
};

#include "ui_SeerEditorWidgetSource.h"

class SeerEditorWidgetSource : public QWidget, protected Ui::SeerEditorWidgetSourceForm {

    Q_OBJECT

    public:
        explicit SeerEditorWidgetSource (QWidget* parent = 0);
       ~SeerEditorWidgetSource ();

        SeerEditorWidgetSourceArea*                 sourceArea                              ();

        bool                                        isSearchBarShown                        () const;
        bool                                        searchMatchCase                         () const;
        bool                                        isAlternateBarShown                     () const;
        void                                        setKeySettings                          (const SeerKeySettings& settings);
        const SeerKeySettings&                      keySettings                             () const;

    public slots:
        void                                        showSearchBar                           (bool flag);
        void                                        setSearchMatchCase                      (bool flag);
        void                                        showAlternateBar                        (bool flag);
        void                                        showReloadBar                           (bool flag);

    private slots:
        void                                        handleSearchLineNumberLineEdit          ();
        void                                        handleSearchTextLineEdit                ();
        void                                        handleSearchDownToolButton              ();
        void                                        handleSearchUpToolButton                ();
        void                                        handleSearchCloseToolButton             ();
        void                                        handleAlternateCloseToolButton          ();
        void                                        handleAlternateFileOpenToolButton       ();
        void                                        handleAlternateLineEdit                 ();
        void                                        handleTextSearchShortcut                ();
        void                                        handleAlternateDirectoryShortcut        ();
        void                                        handleReloadToolButton                  ();
        void                                        handleReloadCloseToolButton             ();

    signals:
        void                                        addAlternateDirectory                   (QString path);

    private:
        SeerKeySettings                             _keySettings;
        QShortcut*                                  _textSearchShortcut;
        QShortcut*                                  _textSearchNextShortcut;
        QShortcut*                                  _textSearchPrevShortcut;
        QShortcut*                                  _textSearchReloadShortcut;
        QShortcut*                                  _alternateDirShortcut;
};

