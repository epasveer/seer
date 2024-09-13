#pragma once

#include <QtWidgets/QWidget>

#include "ui_SeerCudaManagerWidget.h"

class SeerCudaManagerWidget : public QWidget, protected Ui::SeerCudaManagerWidgetForm {

    Q_OBJECT

    public:
        explicit SeerCudaManagerWidget (QWidget* parent = 0);
       ~SeerCudaManagerWidget ();

    signals:

    protected:
        void                                            writeSettings                       ();
        void                                            readSettings                        ();

    public slots:

    private slots:
        void                                            handleRefreshToolButtonClicked      ();
        void                                            handleHelpToolButtonClicked         ();
        void                                            handleTabMoved                      (int from, int to);
        void                                            handleTabChanged                    (int index);

    private:
};

