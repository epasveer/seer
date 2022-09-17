#pragma once

#include "SeerThreadIdsBrowserWidget.h"
#include "SeerThreadFramesBrowserWidget.h"

#include <QtWidgets/QWidget>

#include "ui_SeerThreadManagerWidget.h"

class SeerThreadManagerWidget : public QWidget, protected Ui::SeerThreadManagerWidgetForm {

    Q_OBJECT

    public:
        explicit SeerThreadManagerWidget (QWidget* parent = 0);
       ~SeerThreadManagerWidget ();

        SeerThreadIdsBrowserWidget*                     threadIdsBrowserWidget              ();
        SeerThreadFramesBrowserWidget*                  threadFramesBrowserWidget           ();

    signals:
        void                                            forkFollowsModeChanged              (const QString& mode);


    public slots:
        void                                            setForkFollowsMode                  (const QString& mode);
        QString                                         forkFollowsMode                     () const;


    private slots:
        void                                            handleRefreshToolButtonClicked      ();
        void                                            handleForkFollowComboBox            (int index);

    private:
        SeerThreadIdsBrowserWidget*                     _threadIdsBrowserWidget;
        SeerThreadFramesBrowserWidget*                  _threadFramesBrowserWidget;
};

