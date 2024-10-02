#pragma once

#include <QtWidgets/QDialog>
#include <QtCore/QString>

#include "ui_SeerStackDumpSettingsDialog.h"

class SeerStackDumpSettingsDialog : public QDialog, protected Ui::SeerStackDumpSettingsDialogForm {

    Q_OBJECT

    public:
        explicit SeerStackDumpSettingsDialog (QWidget* parent = 0);
       ~SeerStackDumpSettingsDialog ();

        void                        setStackPointerExpression   (const QString& expression);
        QString                     stackPointerExpression      () const;

        void                        setBytesBeforeSP            (int nbytes);
        int                         bytesBeforeSP               () const;

        void                        setBytesAfterSP             (int nbytes);
        int                         bytesAfterSP                () const;

        void                        setAsciiBytes               (int nbytes);
        int                         asciiBytes                  () const;

    public slots:

    private slots:

    protected:

    private:
};

