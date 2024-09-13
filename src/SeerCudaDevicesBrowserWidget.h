#pragma once

#include <QtWidgets/QWidget>
#include <QtCore/QString>
#include "ui_SeerCudaDevicesBrowserWidget.h"

class SeerCudaDevicesBrowserWidget : public QWidget, protected Ui::SeerCudaDevicesBrowserWidgetForm {

    Q_OBJECT

    public:
        explicit SeerCudaDevicesBrowserWidget (QWidget* parent = 0);
       ~SeerCudaDevicesBrowserWidget ();

    public slots:
        void                handleText                      (const QString& text);
        void                handleStoppingPointReached      ();
        void                refresh                         ();

    protected slots:
        void                handleItemClicked               (QTreeWidgetItem* item, int column);

    signals:
        void                refreshCudaDevices              ();
        void                selectedCudaDevice              (int deviceid);

    protected:
        void                showEvent                       (QShowEvent* event);

    private:
};

