#pragma once

#include "SeerSourceBrowserWidget.h"
#include "SeerFunctionBrowserWidget.h"
#include "SeerTypeBrowserWidget.h"
#include "SeerVariableBrowserWidget.h"
#include "SeerLibraryBrowserWidget.h"

#include <QtWidgets/QWidget>

#include "ui_SeerSourceLibraryManagerWidget.h"

class SeerSourceLibraryManagerWidget : public QWidget, protected Ui::SeerSourceLibraryManagerWidgetForm {

    Q_OBJECT

    public:
        explicit SeerSourceLibraryManagerWidget (QWidget* parent = 0);
       ~SeerSourceLibraryManagerWidget ();

        SeerSourceBrowserWidget*                        sourceBrowserWidget             ();
        SeerFunctionBrowserWidget*                      functionBrowserWidget           ();
        SeerTypeBrowserWidget*                          typeBrowserWidget               ();
        SeerVariableBrowserWidget*                      variableBrowserWidget           ();
        SeerLibraryBrowserWidget*                       libraryBrowserWidget            ();

    signals:
    public slots:
    private slots:
        void                                            handleRefreshToolButtonClicked  ();

    private:
        SeerSourceBrowserWidget*                        _sourceBrowserWidget;
        SeerFunctionBrowserWidget*                      _functionBrowserWidget;
        SeerTypeBrowserWidget*                          _typeBrowserWidget;
        SeerVariableBrowserWidget*                      _variableBrowserWidget;
        SeerLibraryBrowserWidget*                       _libraryBrowserWidget;
};

