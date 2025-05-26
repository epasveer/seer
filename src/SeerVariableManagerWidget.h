#pragma once

#include "SeerVariableTrackerBrowserWidget.h"
#include "SeerVariableLoggerBrowserWidget.h"
#include "SeerRegisterValuesBrowserWidget.h"

#include <QtWidgets/QWidget>

#include "ui_SeerVariableManagerWidget.h"

class SeerVariableManagerWidget : public QWidget, protected Ui::SeerVariableManagerWidgetForm {

    Q_OBJECT

    public:
        explicit SeerVariableManagerWidget (QWidget* parent = 0);
       ~SeerVariableManagerWidget ();

        SeerVariableTrackerBrowserWidget*               variableTrackerBrowserWidget        ();
        SeerVariableLoggerBrowserWidget*                variableLoggerBrowserWidget           ();
        SeerRegisterValuesBrowserWidget*                registerValuesBrowserWidget         ();

    protected:
        void                                            writeSettings                       ();
        void                                            readSettings                        ();

    private slots:
        void                                            handleRefreshToolButtonClicked      ();
        void                                            handleHelpToolButtonClicked         ();
        void                                            handleTabMoved                      (int from, int to);
        void                                            handleTabChanged                    (int index);
        void                                            handleRaiseLoggerTab                ();
        void                                            handleRaiseTrackerTab               ();

    private:
        SeerVariableTrackerBrowserWidget*               _variableTrackerBrowserWidget;
        SeerVariableLoggerBrowserWidget*                _variableLoggerBrowserWidget;
        SeerRegisterValuesBrowserWidget*                _registerValuesBrowserWidget;
};

