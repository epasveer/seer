#pragma once

#include "SeerEditorWidgetAssembly.h"
#include "SeerEditorManagerEntry.h"
#include "SeerHighlighterSettings.h"
#include "SeerKeySettings.h"
#include <QtGui/QFont>
#include <QtWidgets/QWidget>
#include <QtCore/QMap>
#include <QtCore/QString>
#include <QtCore/QStringList>

#include "ui_SeerEditorManagerWidget.h"

class SeerEditorManagerWidget : public QWidget, protected Ui::SeerEditorManagerWidgetForm {

    Q_OBJECT

    public:
        explicit SeerEditorManagerWidget (QWidget* parent = 0);
       ~SeerEditorManagerWidget ();

        void                                            dumpEntries                         () const;
        bool                                            hasEntry                            (const QString& fullname) const;
        SeerEditorManagerEntries::iterator              addEntry                            (const QString& fullname, const QString& file);
        SeerEditorManagerEntries::iterator              findEntry                           (const QString& fullname);
        SeerEditorManagerEntries::const_iterator        findEntry                           (const QString& fullname) const;
        SeerEditorManagerEntries::iterator              beginEntry                          ();
        SeerEditorManagerEntries::const_iterator        beginEntry                          () const;
        SeerEditorManagerEntries::iterator              endEntry                            ();
        SeerEditorManagerEntries::const_iterator        endEntry                            () const;
        void                                            deleteEntry                         (SeerEditorManagerEntries::iterator i);

        void                                            showAssembly                        ();
        SeerEditorWidgetAssembly*                       assemblyWidgetTab                   ();
        void                                            setKeepAssemblyTabOnTop             (bool flag);
        bool                                            keepAssemblyTabOnTop                () const;
        void                                            setAssemblyShowAddressColumn        (bool flag);
        bool                                            assemblyShowAddressColumn           () const;
        void                                            setAssemblyShowOffsetColumn         (bool flag);
        bool                                            assemblyShowOffsetColumn            () const;
        void                                            setAssemblyShowOpcodeColumn         (bool flag);
        bool                                            assemblyShowOpcodeColumn            () const;
        void                                            setAssemblyShowSourceLines          (bool flag);
        bool                                            assemblyShowSourceLines             () const;


        SeerEditorManagerFiles                          openedFiles                         () const;

        void                                            setEditorFont                       (const QFont& font);
        const QFont&                                    editorFont                          () const;
        void                                            setEditorHighlighterSettings        (const SeerHighlighterSettings& settings);
        const SeerHighlighterSettings&                  editorHighlighterSettings           () const;
        void                                            setEditorHighlighterEnabled         (bool flag);
        bool                                            editorHighlighterEnabled            () const;
        void                                            setEditorAlternateDirectories       (const QStringList alternateDirectories);
        const QStringList&                              editorAlternateDirectories          () const;
        void                                            setEditorIgnoreDirectories          (const QStringList ignoreDirectories);
        const QStringList&                              editorIgnoreDirectories             () const;
        void                                            setEditorKeySettings                (const SeerKeySettings& settings);
        const SeerKeySettings&                          editorKeySettings                   () const;
        void                                            setEditorTabSize                    (int spaces);
        int                                             editorTabSize                       () const;
        void                                            setEditorExternalEditorCommand      (const QString& externalEditorCommand);
        const QString&                                  editorExternalEditorCommand         () const;

    public slots:
        void                                            handleText                          (const QString& text);
        void                                            handleTabCloseRequested             (int index);
        void                                            handleTabCurrentChanged             (int index);
        void                                            handleOpenFile                      (const QString& file, const QString& fullname, int lineno);
        void                                            handleOpenAddress                   (const QString& address);
        void                                            handleInsertBreakpoint              (QString breakpoint);
        void                                            handleInsertPrintpoint              (QString printpoint);
        void                                            handleDeleteBreakpoints             (QString breakpoints);
        void                                            handleEnableBreakpoints             (QString breakpoints);
        void                                            handleDisableBreakpoints            (QString breakpoints);
        void                                            handleRefreshBreakpointsStackFrames ();
        void                                            handleRunToLine                     (QString fullname, int lineno);
        void                                            handleRunToAddress                  (QString address);
        void                                            handleAddVariableLoggerExpression   (QString expression);
        void                                            handleAddVariableTrackerExpression  (QString expression);
        void                                            handleRefreshVariableTrackerValues  ();
        void                                            handleEvaluateVariableExpression    (int expressionid, QString expression);
        void                                            handleAddMemoryVisualizer           (QString expression);
        void                                            handleAddArrayVisualizer            (QString expression);
        void                                            handleAddStructVisualizer           (QString expression);
        void                                            handleRequestAssembly               (QString address);
        void                                            handleRequestSourceAndAssembly      (QString address);
        void                                            handleAssemblyConfigChanged         ();

    private slots:
        void                                            handleFileOpenToolButtonClicked     ();
        void                                            handleFileCloseToolButtonClicked    ();
        void                                            handleTextSearchToolButtonClicked   ();
        void                                            handleHelpToolButtonClicked         ();
        void                                            handleAddAlternateDirectory         (QString path);

    signals:
        void                                            refreshBreakpointsList              ();
        void                                            refreshStackFrames                  ();
        void                                            insertBreakpoint                    (QString breakpoint);
        void                                            insertPrintpoint                    (QString printpoint);
        void                                            deleteBreakpoints                   (QString breakpoints);
        void                                            enableBreakpoints                   (QString breakpoints);
        void                                            disableBreakpoints                  (QString breakpoints);
        void                                            runToLine                           (QString file, int lineno);
        void                                            runToAddress                        (QString address);
        void                                            addVariableLoggerExpression         (QString expression);
        void                                            addVariableTrackerExpression        (QString expression);
        void                                            refreshVariableTrackerValues        ();
        void                                            evaluateVariableExpression          (int expressionid, QString expression);
        void                                            addMemoryVisualize                  (QString expression);
        void                                            addArrayVisualize                   (QString expression);
        void                                            addStructVisualize                  (QString expression);
        void                                            requestAssembly                     (QString address);
        void                                            requestSourceAndAssembly            (QString address);
        void                                            showMessage                         (QString message, int time);
        void                                            assemblyTabShown                    (bool shown);

    private:
        SeerEditorWidgetSource*                         currentEditorWidgetTab              ();
        SeerEditorWidgetSource*                         editorWidgetTab                     (const QString& fullname);
        SeerEditorWidgetSource*                         createEditorWidgetTab               (const QString& fullname, const QString& file, const QString& text);
        SeerEditorWidgetSource*                         createEditorWidgetTab               (const QString& fullname, const QString& file);
        void                                            deleteEditorWidgetTab               (int index);
        SeerEditorWidgetAssembly*                       createAssemblyWidgetTab             ();
        void                                            deleteAssemblyWidgetTab             ();

        SeerEditorManagerEntries                        _entries;
        SeerHighlighterSettings                         _editorHighlighterSettings;
        bool                                            _editorHighlighterEnabled;
        QFont                                           _editorFont;
        QStringList                                     _editorAlternateDirectories;
        QStringList                                     _editorIgnoreDirectories;
        SeerKeySettings                                 _editorKeySettings;
        int                                             _editorTabSize;
        QString                                         _editorExternalEditorCommand;
        SeerEditorWidgetAssembly*                       _assemblyWidget;
        int                                             _assemblyIndex;
        bool                                            _keepAssemblyTabOnTop;
        bool                                            _showAddressColumn;
        bool                                            _showOffsetColumn;
        bool                                            _showOpcodeColumn;
        bool                                            _showSourceLines;
        bool                                            _notifyAssemblyTabShown;
};

