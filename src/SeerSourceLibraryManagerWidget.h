#pragma once

#include "SeerSourceBrowserWidget.h"
#include "SeerSharedLibraryBrowserWidget.h"

#include <QtWidgets/QWidget>

#include "ui_SeerSourceLibraryManagerWidget.h"

class SeerSourceLibraryManagerWidget : public QWidget, protected Ui::SeerSourceLibraryManagerWidgetForm {

    Q_OBJECT

    public:
        explicit SeerSourceLibraryManagerWidget (QWidget* parent = 0);
       ~SeerSourceLibraryManagerWidget ();

        SeerSourceBrowserWidget*                        sourceBrowserWidget             ();
        SeerSharedLibraryBrowserWidget*                 sharedLibraryBrowserWidget      ();

    signals:
    public slots:
    private slots:
        void                                            handleRefreshToolButtonClicked  ();

    private:
        SeerSourceBrowserWidget*                        _sourceBrowserWidget;
        SeerSharedLibraryBrowserWidget*                 _sharedLibraryBrowserWidget;
};

