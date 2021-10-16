#pragma once

#include <QList>
#include <QString>

class QProcessInfo;
typedef QList<QProcessInfo> QProcessList;

class QProcessInfo {
    public:
        QProcessInfo();

        static QProcessList enumerate();

        uint32_t            pid                 () const;
        void                setPid              (uint32_t pid);

        const QString&      name                () const;
        void                setName             (const QString& name);

        const QString&      commandLine         () const;
        void                setCommandLine      (const QString& cmd);

    private:
        uint32_t            _pid;
        QString             _name;
        QString             _cmdLine;
};

