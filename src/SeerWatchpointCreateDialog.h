#pragma once

#include <QtWidgets/QDialog>
#include <QtCore/QString>

#include "ui_SeerWatchpointCreateDialog.h"

class SeerWatchpointCreateDialog : public QDialog, protected Ui::SeerWatchpointCreateDialogForm {

    Q_OBJECT

    public:
        explicit SeerWatchpointCreateDialog (QWidget* parent = 0);
       ~SeerWatchpointCreateDialog ();

        void            setExpression               (const QString& text);

        QString         expressionText              () const;

        void            setReadAccessEnabled        (bool flag);
        void            setWriteAccessEnabled       (bool flag);
        void            setReadWriteAccessEnabled   (bool flag);

        bool            readAccessEnabled           () const;
        bool            writeAccessEnabled          () const;
        bool            readWriteAccessEnabled      () const;

        QString         watchpointText              () const;

    public slots:

    private:
};

