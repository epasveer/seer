#pragma once

#include <QtWidgets/QDialog>
#include <QtCore/QString>

#include "ui_SeerPrintpointCreateDialog.h"

class SeerPrintpointCreateDialog : public QDialog, protected Ui::SeerPrintpointCreateDialogForm {

    Q_OBJECT

    public:
        explicit SeerPrintpointCreateDialog (QWidget* parent = 0);
       ~SeerPrintpointCreateDialog ();

        void            setFilename                 (const QString& text);
        void            setFunctionName             (const QString& text);
        void            setLabelName                (const QString& text);
        void            setLineNumber               (const QString& text);

        QString         filenameText                () const;
        QString         functionNameText            () const;
        QString         labelNameText               () const;
        QString         lineNumberText              () const;

        void            setTemporaryEnabled         (bool flag);
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

        void            setFormat                   (const QString& text);
        void            setArguments                (const QString& text);

        QString         format                      () const;
        QString         arguments                   () const;

        QString         dprintfType                 () const;
        QString         dprintfFunction             () const;
        QString         dprintfChannel              () const;

        void            setDPrintfType              (const QString& text);
        void            setDPrintfFunction          (const QString& text);
        void            setDPrintfChannel           (const QString& text);

        QString         printpointParameters        () const;

    public slots:
        void            handleDprintfTypeChanged    ();

    private slots:
        void            handleHelpToolButtonClicked ();

    private:
};

