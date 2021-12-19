#pragma once

#include <QtWidgets/QWidget>

#include "ui_SeerGdbConfigPage.h"

class SeerGdbConfigPage : public QWidget, public Ui::SeerGdbConfigPage {

    Q_OBJECT

    public:
        explicit SeerGdbConfigPage (QWidget* parent = 0);
       ~SeerGdbConfigPage ();

};

