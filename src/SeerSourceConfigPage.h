#pragma once

#include <QtWidgets/QWidget>

#include "ui_SeerSourceConfigPage.h"

class SeerSourceConfigPage : public QWidget, public Ui::SeerSourceConfigPage {

    Q_OBJECT

    public:
        explicit SeerSourceConfigPage (QWidget* parent = 0);
       ~SeerSourceConfigPage ();

};

