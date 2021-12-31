#pragma once

#include <QtWidgets/QDialog>
#include <QtCore/QString>

#include "ui_SeerCatchpointCreateDialog.h"

class SeerCatchpointCreateDialog : public QDialog, protected Ui::SeerCatchpointCreateDialogForm {

    Q_OBJECT

    public:
        explicit SeerCatchpointCreateDialog (QWidget* parent = 0);
       ~SeerCatchpointCreateDialog ();

        void            setType                     (const QString& text);
        QString         typeText                    () const;

        void            setTemporaryEnabled         (bool flag);
        bool            temporaryEnabled            () const;

        void            setNameEnabled              (bool flag);
        bool            nameEnabled                 () const;

        void            setNameText                 (const QString& text);
        QString         nameText                    () const;

        QString         catchpointText              () const;

    public slots:

    private:
};

