#pragma once

#include <QtWidgets/QDialog>
#include <QtCore/QString>

#include "ui_SeerArgumentsDialog.h"

class SeerArgumentsDialog : public QDialog, protected Ui::SeerArgumentsDialogForm {

    Q_OBJECT

    public:
        explicit SeerArgumentsDialog (QWidget* parent = 0);
       ~SeerArgumentsDialog ();

        void                    setExecutableArguments              (const QString& executableArguments);
        QString                 executableArguments                 ();

    public slots:

    private:
};

