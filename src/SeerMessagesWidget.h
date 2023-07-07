#pragma once

#include <QtWidgets/QMessageBox>
#include <QtWidgets/QWidget>
#include <QtGui/QIcon>
#include <QtCore/QString>
#include "ui_SeerMessagesWidget.h"

class SeerMessagesWidget : public QWidget, protected Ui::SeerMessagesWidget {

    Q_OBJECT

    public:
        explicit SeerMessagesWidget (QWidget* parent = 0);
       ~SeerMessagesWidget ();

    signals:

    public slots:
        void                addMessage                      (const QString& message, QMessageBox::Icon messageType);

    protected slots:
        void                handleOkButtonClicked           ();

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

