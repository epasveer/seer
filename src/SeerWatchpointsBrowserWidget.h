#pragma once

#include <QtWidgets/QWidget>
#include <QtCore/QString>
#include <QtCore/QStringList>
#include "ui_SeerWatchpointsBrowserWidget.h"

class SeerWatchpointsBrowserWidget : public QWidget, protected Ui::SeerWatchpointsBrowserWidgetForm {

    Q_OBJECT

    public:
        explicit SeerWatchpointsBrowserWidget (QWidget* parent = 0);
       ~SeerWatchpointsBrowserWidget ();

        bool                isEmpty                     () const;
        QStringList         breakpoints                 () const;

    public slots:
        void                handleText                  (const QString& text);
        void                handleStoppingPointReached  ();
        void                handleSessionTerminated     ();

    private slots:
        void                handleItemDoubleClicked     (QTreeWidgetItem* item, int column);
        void                handleRefreshToolButton     ();
        void                handleAddToolButton         ();
        void                handleDeleteToolButton      ();
        void                handleEnableToolButton      ();
        void                handleDisableToolButton     ();
        void                handleConditionToolButton   ();
        void                handleIgnoreToolButton      ();
        void                handleCommandsToolButton    ();

    signals:
        void                refreshWatchpointsList      ();
        void                deleteWatchpoints           (QString watchpoints);
        void                enableWatchpoints           (QString watchpoints);
        void                disableWatchpoints          (QString watchpoints);
        void                addBreakpointCondition      (QString watchpoint, QString condition);
        void                addBreakpointIgnore         (QString watchpoint, QString count);
        void                addBreakpointCommands       (QString watchpoint, QStringList commands);
        void                insertWatchpoint            (QString watchpoint);
        void                selectedFile                (QString file, QString fullname, int lineno);

    protected:
        void                showEvent                   (QShowEvent* event);

    private:
};

