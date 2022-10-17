#ifndef GdbMonitor_H
#define GdbMonitor_H

#include <QtCore/QObject>
#include <QtCore/QtCore>

class GdbMonitor : public QObject {

    Q_OBJECT

    public:
        explicit GdbMonitor (QObject* parent = 0);
        virtual ~GdbMonitor ();

        void            setProcess                      (QProcess* process);
        QProcess*       process                         ();

    signals:
        void            allTextOutput                   (const QString& text);
        void            tildeTextOutput                 (const QString& text);
        void            equalTextOutput                 (const QString& text);
        void            astrixTextOutput                (const QString& text);
        void            caretTextOutput                 (const QString& text);
        void            ampersandTextOutput             (const QString& text);
        void            atsignTextOutput                (const QString& text);
        void            textOutput                      (const QString& text);

    public slots:
        void            handleErrorOccurred             (QProcess::ProcessError error);
        void            handleFinished                  (int exitCode, QProcess::ExitStatus exitStatus);
        void            handleReadyReadStandardError    ();
        void            handleReadyReadStandardOutput   ();
        void            handleStarted                   ();
        void            handleStateChanged              (QProcess::ProcessState newState);
        void            handleTextOutput                (QString text);

    private:
        QProcess*       _process;
};

#endif

