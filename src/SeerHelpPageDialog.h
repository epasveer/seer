#pragma once

#include "ui_SeerHelpPageDialog.h"

class SeerHelpPageDialog: public QDialog, protected Ui::SeerHelpPageDialogForm {

    Q_OBJECT

    public:

        SeerHelpPageDialog(QDialog* parent = 0);
       ~SeerHelpPageDialog();

        void                        loadFile                            (const QString& filename);
        void                        loadText                            (const QString& text);

    signals:
    public slots:
    protected:
        void                        writeSettings                       ();
        void                        readSettings                        ();
        void                        resizeEvent                         (QResizeEvent* event);

    protected slots:
    private:
};

