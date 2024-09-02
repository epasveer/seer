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

        void                setMode                     (const QString& mode);
        QString             mode                        () const;
        void                enableStdout                (bool flag);
        bool                isStdoutEnabled             () const;
        void                enableWrap                  (bool flag);
        bool                isWrapEnabled               () const;

    public slots:
        void                handleChangeWindowTitle     (QString title);
        void                handleTabDetached           (QWidget* widget);
        void                handleTabReattached         (QWidget* widget);

    signals:
        void                modeChanged                 (QString mode);

    protected slots:
        void                handleClearButton           ();
        void                handlePrintButton           ();
        void                handleSaveButton            ();
        void                handleFontButton            ();
        void                handleWrapTextCheckBox      ();
        void                handleStdoutCheckBox        ();
        void                handleStdinLineEdit         ();
        void                handleConsoleOutput         (int socketfd);

    protected:
        void                handleText                  (const char* buffer, int count);
        void                createConsole               ();
        void                deleteConsole               ();
        void                writeSettings               ();
        void                writeModeSettings           ();
        void                writeFontSettings           ();
        void                writeSizeSettings           ();
        void                readSettings                ();
        void                resizeEvent                 (QResizeEvent* event);

    private:
        QString             _mode;
        QString             _ttyDeviceName;
        int                 _ptsFD;
        QSocketNotifier*    _ptsListener;
        bool                _enableStdout;
        bool                _enableWrap;
};

