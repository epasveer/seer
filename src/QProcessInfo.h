#pragma once

#include <QList>
#include <QString>

class QProcessInfo;
typedef QList<QProcessInfo> QProcessList;

class QProcessInfo {
    public:
        QProcessInfo();

        static QProcessList populate();

        uint32_t            pid                 () const;
        void                setPid              (uint32_t pid);

        const QString&      username            () const;
        void                setUsername         (const QString& username);

        const QString&      name                () const;
        void                setName             (const QString& name);

        const QString&      commandLine         () const;
        void                setCommandLine      (const QString& cmd);

    private:
        uint32_t            _pid;
        QString             _username;
        QString             _name;
        QString             _cmdLine;
};

