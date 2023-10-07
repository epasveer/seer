#pragma once

#include <QtWidgets/QMessageBox>
#include <QtWidgets/QWidget>
#include <QtGui/QIcon>
#include <QtCore/QString>
#include "ui_SeerMessagesBrowserWidget.h"

class SeerMessagesBrowserWidget : public QWidget, protected Ui::SeerMessagesBrowserWidgetForm {

    Q_OBJECT

    public:
        explicit SeerMessagesBrowserWidget (QWidget* parent = 0);
       ~SeerMessagesBrowserWidget ();

    public slots:
        void                addMessage                  (const QString& message, QMessageBox::Icon messageType);
        void                clearMessages               ();

    private slots:
        void                handleDeleteToolButton      ();

    signals:
        void                showMessages                ();

    protected:

    private:
        QIcon               _noIcon;
        QIcon               _informationIcon;
        QIcon               _warningIcon;
        QIcon               _criticalIcon;
        QIcon               _questionIcon;
};

