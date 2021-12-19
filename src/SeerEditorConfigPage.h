#pragma once

#include <QtWidgets/QWidget>

#include "ui_SeerEditorConfigPage.h"

class SeerEditorConfigPage : public QWidget, protected Ui::SeerEditorConfigPage {

    Q_OBJECT

    public:
        explicit SeerEditorConfigPage (QWidget* parent = 0);
       ~SeerEditorConfigPage ();

};

