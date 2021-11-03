#pragma once

#include "SeerEditorManagerEntry.h"
#include <QtWidgets/QWidget>
#include <QtCore/QMap>
#include <QtCore/QString>

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

    public slots:
        void                                            handleText                          (const QString& text);
        void                                            handleTabCloseRequested             (int index);
        void                                            handleOpenFile                      (const QString& file, const QString& fullname, int lineno);
        void                                            handleInsertBreakpoint              (QString breakpoint);
        void                                            handleDeleteBreakpoints             (QString breakpoints);
        void                                            handleEnableBreakpoints             (QString breakpoints);
        void                                            handleDisableBreakpoints            (QString breakpoints);
        void                                            handleRunToLine                     (QString fullname, int lineno);
        void                                            handleAddVariableLoggerExpression   (QString expression);
        void                                            handleAddVariableTrackerExpression  (QString expression);
        void                                            handleRefreshVariableTrackerValues  ();
        void                                            handleEvaluateVariableExpression    (int expressionid, QString expression);
        void                                            handleAddMemoryVisualizer           (QString expression);

    private slots:
        void                                            handleFileOpenToolButtonClicked     ();
        void                                            handleTextSearchToolButtonClicked   ();

    signals:
        void                                            refreshBreakpointsList              ();
        void                                            refreshStackFrames                  ();
        void                                            insertBreakpoint                    (QString breakpoint);
        void                                            deleteBreakpoints                   (QString breakpoints);
        void                                            enableBreakpoints                   (QString breakpoints);
        void                                            disableBreakpoints                  (QString breakpoints);
        void                                            runToLine                           (QString file, int lineno);
        void                                            addVariableLoggerExpression         (QString expression);
        void                                            addVariableTrackerExpression        (QString expression);
        void                                            refreshVariableTrackerValues        ();
        void                                            evaluateVariableExpression          (int expressionid, QString expression);
        void                                            addMemoryVisualize                  (QString expression);

    private:
        SeerEditorWidget*                               currentEditorWidgetTab              ();
        SeerEditorWidget*                               editorWidgetTab                     (const QString& fullname);
        SeerEditorWidget*                               createEditorWidgetTab               (const QString& fullname, const QString& file, const QString& text);
        SeerEditorWidget*                               createEditorWidgetTab               (const QString& fullname, const QString& file);
        void                                            deleteEditorWidgetTab               (int index);

        SeerEditorManagerEntries                        _entries;
};

