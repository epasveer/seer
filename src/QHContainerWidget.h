#pragma once

#include <QtWidgets/QWidget>
#include "ui_QHContainerWidget.h"

class QHContainerWidget : public QWidget, protected Ui::QHContainerWidgetForm {

    Q_OBJECT

    public:
        explicit QHContainerWidget (QWidget* parent = 0);
       ~QHContainerWidget ();

        void                addWidget                   (QWidget* widget);
        void                removeWidget                (QWidget* widget);

    private:
};

