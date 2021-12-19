#pragma once

#include <QtWidgets/QDialog>

class QListWidget;
class QListWidgetItem;
class QStackedWidget;

#include "ui_SeerConfigDialog.h"

class SeerConfigDialog : public QDialog, protected Ui::SeerConfigDialogForm {

    Q_OBJECT

    public:
        explicit SeerConfigDialog (QWidget* parent = 0);
       ~SeerConfigDialog ();

    public slots:
        void                    changePage              (QListWidgetItem* current, QListWidgetItem* previous);

    private:

};

