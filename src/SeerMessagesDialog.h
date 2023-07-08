#pragma once

#include <QtWidgets/QMessageBox>
#include <QtWidgets/QDialog>
#include <QtGui/QIcon>
#include <QtCore/QString>
#include "ui_SeerMessagesDialog.h"

class SeerMessagesDialog : public QDialog, protected Ui::SeerMessagesDialog {

    Q_OBJECT

    public:
        explicit SeerMessagesDialog (QWidget* parent = 0);
       ~SeerMessagesDialog ();

    signals:

    public slots:
        void                addMessage                      (const QString& message, QMessageBox::Icon messageType);
        void                clearMessages                   ();

    protected slots:

    protected:
        void                writeSettings                   ();
        void                readSettings                    ();
        void                resizeEvent                     (QResizeEvent* event);

    private:
        QIcon               _noIcon;
        QIcon               _informationIcon;
        QIcon               _warningIcon;
        QIcon               _criticalIcon;
        QIcon               _questionIcon;
};

