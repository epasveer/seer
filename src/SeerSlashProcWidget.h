#pragma once

#include <QtWidgets/QWidget>
#include <QtCore/QString>
#include "ui_SeerSlashProcWidget.h"

class SeerSlashProcWidget : public QWidget, protected Ui::SeerSlashProcWidgetForm {

    Q_OBJECT

    public:
        explicit SeerSlashProcWidget (QWidget* parent = 0);
       ~SeerSlashProcWidget ();

        int                 selectedPid                     () const;
        QString             selectedUsername                () const;
        QString             selectedName                    () const;
        QString             selectedCommandLine             () const;

    signals:
        void                pidSelected                     ();

    public slots:

    protected slots:
        void                refreshList                     ();
        void                refreshView                     ();
        void                handleDoubleClicked             ();

    protected:
    private:
};

