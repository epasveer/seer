#pragma once

#include <QtWidgets/QWidget>

#include "ui_SeerAssemblyConfigPage.h"

class SeerAssemblyConfigPage : public QWidget, protected Ui::SeerAssemblyConfigPage {

    Q_OBJECT

    public:
        explicit SeerAssemblyConfigPage (QWidget* parent = 0);
       ~SeerAssemblyConfigPage ();

    protected slots:
};

