#pragma once

#include <QtWidgets/QWidget>
#include <QtCore/QString>
#include "ui_SeerCatchpointsBrowserWidget.h"

class SeerCatchpointsBrowserWidget : public QWidget, protected Ui::SeerCatchpointsBrowserWidgetForm {

    Q_OBJECT

    public:
        explicit SeerCatchpointsBrowserWidget (QWidget* parent = 0);
       ~SeerCatchpointsBrowserWidget ();

        bool                isEmpty                     () const;

    public slots:
        void                handleText                  (const QString& text);
        void                handleStoppingPointReached  ();

    private slots:
        void                handleRefreshToolButton     ();
        void                handleAddToolButton         ();
        void                handleDeleteToolButton      ();
        void                handleEnableToolButton      ();
        void                handleDisableToolButton     ();

    signals:
        void                refreshCatchpointsList      ();
        void                deleteCatchpoints           (QString catchpoints);
        void                enableCatchpoints           (QString catchpoints);
        void                disableCatchpoints          (QString catchpoints);
        void                insertCatchpoint            (QString catchpoint);

    protected:
        void                showEvent                   (QShowEvent* event);

    private:
};

