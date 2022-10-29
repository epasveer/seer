#pragma once

#include <QtCore/QByteArray>
#include "ui_SeerImageWidget.h"

class SeerImageWidget: public QWidget, protected Ui::SeerImageWidgetForm {

    Q_OBJECT

    public:

        SeerImageWidget(QWidget* parent = 0);
       ~SeerImageWidget();

    signals:

    public slots:
        void                        setData                             (const QByteArray& data);
        void                        refresh                             ();

    protected:

    protected slots:
        void                        handleRedrawToolButton              ();

    private:
        QByteArray                  _data;
};

