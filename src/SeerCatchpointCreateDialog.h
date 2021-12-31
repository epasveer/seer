#pragma once

#include <QtWidgets/QDialog>
#include <QtCore/QString>

#include "ui_SeerCatchpointCreateDialog.h"

class SeerCatchpointCreateDialog : public QDialog, protected Ui::SeerCatchpointCreateDialogForm {

    Q_OBJECT

    public:
        explicit SeerCatchpointCreateDialog (QWidget* parent = 0);
       ~SeerCatchpointCreateDialog ();

        void            setFilename                 (const QString& text);
        void            setFunctionName             (const QString& text);
        void            setLabelName                (const QString& text);
        void            setLineNumber               (const QString& text);

        QString         filenameText                () const;
        QString         functionNameText            () const;
        QString         labelNameText               () const;
        QString         lineNumberText              () const;

        void            setTemporaryEnabled         (bool flag);
        void            setHardwareEnabled          (bool flag);
        void            setPendingEnabled           (bool flag);
        void            setDisabledEnabled          (bool flag);
        void            setConditionalEnabled       (bool flag);
        void            setIgnoreCountEnabled       (bool flag);
        void            setThreadIdEnabled          (bool flag);

        void            setConditionalText          (const QString& text);
        void            setIgnoreCountText          (const QString& text);
        void            setThreadIdText             (const QString& text);

        bool            temporaryEnabled            () const;
        bool            hardwareEnabled             () const;
        bool            pendingEnabled              () const;
        bool            disabledEnabled             () const;
        bool            conditionalEnabled          () const;
        bool            ignoreCountEnabled          () const;
        bool            threadIdEnabled             () const;

        QString         conditionalText             () const;
        QString         ignoreCountText             () const;
        QString         threadIdText                () const;

        QString         catchpointText              () const;

    public slots:

    private:
};

