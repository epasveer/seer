#pragma once

#include <QtWidgets/QWidget>
#include <QtCore/QString>
#include "ui_SeerSharedLibraryBrowserWidget.h"

class SeerSharedLibraryBrowserWidget : public QWidget, protected Ui::SeerSharedLibraryBrowserWidgetForm {

    Q_OBJECT

    public:
        explicit SeerSharedLibraryBrowserWidget (QWidget* parent = 0);
       ~SeerSharedLibraryBrowserWidget ();

    public slots:
        void                handleText                  (const QString& text);
        void                handleSearchLineEdit        (const QString& text);
        void                refresh                     ();

    signals:
        void                refreshSharedLibraryList    ();

    protected:
        void                showEvent                   (QShowEvent* event);

    private:
};

