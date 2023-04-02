#pragma once

#include "SeerSourceBrowserWidget.h"
#include "SeerFunctionBrowserWidget.h"
#include "SeerTypeBrowserWidget.h"
#include "SeerStaticBrowserWidget.h"
#include "SeerLibraryBrowserWidget.h"
#include "SeerAdaExceptionsBrowserWidget.h"

#include <QtWidgets/QWidget>

#include "ui_SeerSourceSymbolLibraryManagerWidget.h"

class SeerSourceSymbolLibraryManagerWidget : public QWidget, protected Ui::SeerSourceSymbolLibraryManagerWidgetForm {

    Q_OBJECT

    public:
        explicit SeerSourceSymbolLibraryManagerWidget (QWidget* parent = 0);
       ~SeerSourceSymbolLibraryManagerWidget ();

        SeerSourceBrowserWidget*                        sourceBrowserWidget             ();
        SeerFunctionBrowserWidget*                      functionBrowserWidget           ();
        SeerTypeBrowserWidget*                          typeBrowserWidget               ();
        SeerStaticBrowserWidget*                        staticBrowserWidget             ();
        SeerLibraryBrowserWidget*                       libraryBrowserWidget            ();
        SeerAdaExceptionsBrowserWidget*                 adaExceptionsBrowserWidget      ();

    protected:
        void                                            writeSettings                   ();
        void                                            readSettings                    ();

    private slots:
        void                                            handleRefreshToolButtonClicked  ();
        void                                            handleHelpToolButtonClicked     ();
        void                                            handleTabMoved                  (int from, int to);
        void                                            handleTabChanged                (int index);

    private:
        SeerSourceBrowserWidget*                        _sourceBrowserWidget;
        SeerFunctionBrowserWidget*                      _functionBrowserWidget;
        SeerTypeBrowserWidget*                          _typeBrowserWidget;
        SeerStaticBrowserWidget*                        _staticBrowserWidget;
        SeerLibraryBrowserWidget*                       _libraryBrowserWidget;
        SeerAdaExceptionsBrowserWidget*                 _adaExceptionsBrowserWidget;
};

