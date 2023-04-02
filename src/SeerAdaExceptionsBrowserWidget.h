#pragma once

#include <QtWidgets/QWidget>
#include <QtCore/QString>
#include "ui_SeerAdaExceptionsBrowserWidget.h"

class SeerAdaExceptionsBrowserWidget : public QWidget, protected Ui::SeerAdaExceptionsBrowserWidgetForm {

    Q_OBJECT

    public:
        explicit SeerAdaExceptionsBrowserWidget (QWidget* parent = 0);
       ~SeerAdaExceptionsBrowserWidget ();

    public slots:
        void                handleText                  (const QString& text);
        void                refresh                     ();

    protected slots:
        void                handleSearchLineEdit        (const QString& text);

    signals:
        void                refreshAdaExceptions        ();

    protected:
        void                showEvent                   (QShowEvent* event);

    private:
};

