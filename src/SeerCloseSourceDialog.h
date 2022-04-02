#pragma once

#include <QtWidgets/QDialog>
#include <QtWidgets/QWidget>

#include "ui_SeerCloseSourceDialog.h"

class SeerCloseSourceDialog : public QDialog, public Ui::SeerCloseSourceDialogForm {

    Q_OBJECT

    public:
        explicit SeerCloseSourceDialog (QWidget* parent = 0);
       ~SeerCloseSourceDialog ();

    protected slots:

    private:
};

