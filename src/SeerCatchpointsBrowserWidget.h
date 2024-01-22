#pragma once

#include <QtWidgets/QWidget>
#include <QtCore/QString>
#include <QtCore/QStringList>
#include "ui_SeerCatchpointsBrowserWidget.h"

class SeerCatchpointsBrowserWidget : public QWidget, protected Ui::SeerCatchpointsBrowserWidgetForm {

    Q_OBJECT

    public:
        explicit SeerCatchpointsBrowserWidget (QWidget* parent = 0);
       ~SeerCatchpointsBrowserWidget ();

        bool                isEmpty                     () const;
        QStringList         breakpoints                 () const;

    public slots:
        void                handleText                  (const QString& text);
        void                handleStoppingPointReached  ();

    private slots:
        void                handleRefreshToolButton     ();
        void                handleAddToolButton         ();
        void                handleDeleteToolButton      ();
        void                handleEnableToolButton      ();
        void                handleDisableToolButton     ();
        void                handleConditionToolButton   ();
        void                handleIgnoreToolButton      ();
        void                handleCommandsToolButton    ();

    signals:
        void                refreshCatchpointsList      ();
        void                deleteCatchpoints           (QString catchpoints);
        void                enableCatchpoints           (QString catchpoints);
        void                disableCatchpoints          (QString catchpoints);
        void                addBreakpointCondition      (QString catchpoint, QString condition);
        void                addBreakpointIgnore         (QString catchpoint, QString count);
        void                addBreakpointCommands       (QString catchpoint, QStringList commands);
        void                insertCatchpoint            (QString catchpoint);

    protected:
        void                showEvent                   (QShowEvent* event);

    private:
};

