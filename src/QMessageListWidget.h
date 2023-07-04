#pragma once

#include <QtWidgets/QMessageBox>
#include <QtWidgets/QWidget>
#include <QtGui/QIcon>
#include <QtCore/QString>
#include "ui_QMessageListWidget.h"

class QMessageListWidget : public QWidget, protected Ui::QMessageListWidget {

    Q_OBJECT

    public:
        explicit QMessageListWidget (QWidget* parent = 0);
       ~QMessageListWidget ();

    signals:

    public slots:
        void                addMessage                      (const QString& message, QMessageBox::Icon messageType);

    protected slots:
        void                handleOkButtonClicked           ();

    protected:
    private:
        QIcon               _noIcon;
        QIcon               _informationIcon;
        QIcon               _warningIcon;
        QIcon               _criticalIcon;
        QIcon               _questionIcon;
};

