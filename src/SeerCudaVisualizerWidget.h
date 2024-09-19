#pragma once

#include "SeerCudaDevicesBrowserWidget.h"

#include <QtWidgets/QWidget>

#include "ui_SeerCudaVisualizerWidget.h"

class SeerCudaVisualizerWidget : public QWidget, protected Ui::SeerCudaVisualizerWidgetForm {

    Q_OBJECT

    public:
        explicit SeerCudaVisualizerWidget (QWidget* parent = 0);
       ~SeerCudaVisualizerWidget ();

        SeerCudaDevicesBrowserWidget*       cudaDevicesBrowserWidget            ();

    signals:
        void                                refreshCudaDevices                  ();

    protected:
        void                                writeSettings                       ();
        void                                readSettings                        ();
        void                                resizeEvent                         (QResizeEvent* event);

    public slots:
        void                                handleText                          (const QString& text);
        void                                handleStoppingPointReached          ();

    private slots:
        void                                handleRefreshToolButtonClicked      ();
        void                                handleHelpToolButtonClicked         ();
        void                                handleTabMoved                      (int from, int to);
        void                                handleTabChanged                    (int index);
        void                                handleRefreshCudaDevices            ();

    private:
        SeerCudaDevicesBrowserWidget*       _cudaDevicesBrowserWidget;
};

