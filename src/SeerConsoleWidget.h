#pragma once

#include <QtWidgets/QWidget>
#include <QtGui/QTextCursor>
#include <QtGui/QResizeEvent>
#include <QtCore/QString>
#include <QtCore/QSocketNotifier>
#include "ui_SeerConsoleWidget.h"

class SeerConsoleWidget : public QWidget, protected Ui::SeerConsoleWidgetForm {

    Q_OBJECT

    public:
        explicit SeerConsoleWidget (QWidget* parent = 0);
       ~SeerConsoleWidget ();

        const QString&      ttyDeviceName               () const;
        void                connectConsole              ();
        void                disconnectConsole           ();

        void                setScrollLines              (int count);
        int                 scrollLines                 () const;

    protected slots:
        void                handleClearButton           ();
        void                handlePrintButton           ();
        void                handleSaveButton            ();
        void                handleFontButton            ();
        void                handleWrapTextCheckBox      ();
        void                handleStdinLineEdit         ();
        void                handleConsoleOutput         (int socketfd);

    protected:
        void                handleText                  (const char* buffer, int count);
        void                createConsole               ();
        void                deleteConsole               ();
        void                writeSettings               ();
        void                readSettings                ();
        void                resizeEvent                 (QResizeEvent* event);

    private:
        QTextCursor         _cursor;
        QString             _ttyDeviceName;
        int                 _ptsFD;
        QSocketNotifier*    _ptsListener;
};

