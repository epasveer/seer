#pragma once

#include <QtWidgets/QMessageBox>
#include <QtWidgets/QMenu>
#include <QtWidgets/QWidget>
#include <QtGui/QIcon>
#include <QtCore/QString>
#include <QAction>
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
        void                handleRaiseMenuShow         ();
        void                handleRaiseMenuTriggered    (QAction* action);

    signals:
        void                showMessages                ();

    protected:
        void                writeSettings               ();
        void                readSettings                ();

    private:
        QString             _raiseMode;
        QMenu*              _raiseMenu;
        QAction*            _anyMessageAction;
        QAction*            _importanMessagesAction;
        QAction*            _neverMessagesAction;
        QIcon               _noIcon;
        QIcon               _informationIcon;
        QIcon               _warningIcon;
        QIcon               _criticalIcon;
        QIcon               _questionIcon;
};

