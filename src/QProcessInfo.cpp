//
// My version is based on this person's code.
//
// Copyright (c) 2016, Baldur Karlsson
// Licensed under BSD 2-Clause License, see LICENSE file.
// Obtained from https://github.com/baldurk/qprocessinfo
//

#include "QProcessInfo.h"
#include <QtCore/QDir>
#include <QtCore/QProcess>
#include <QtCore/QRegExp>
#include <QtCore/QStandardPaths>
#include <QtCore/QTextStream>
#include <QtCore/QDebug>
#include <stdlib.h>
#include <pwd.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>


QProcessList QProcessInfo::populate() {

    QProcessList ret;

    QDir proc(QStringLiteral("/proc"));

    QStringList files = proc.entryList();

    for (const QString& f : files) {

        bool ok = false;
        uint32_t pid = f.toUInt(&ok);

        if (ok) {

            QProcessInfo info;
            info.setPid(pid);

            QDir processDir(QStringLiteral("/proc/") + f);

            // default to the exe symlink if valid
            QFileInfo exe(processDir.absoluteFilePath(QStringLiteral("exe")));
            exe = QFileInfo(exe.symLinkTarget());
            info.setName(exe.completeBaseName());

            // if we didn't get a name from the symlink, check in the status file
            if (info.name().isEmpty()) {

                QFile status(processDir.absoluteFilePath(QStringLiteral("status")));
                if (status.open(QIODevice::ReadOnly)) {

                    QByteArray contents = status.readAll();

                    QTextStream in(&contents);

                    while(!in.atEnd()) {
                        QString line = in.readLine();

                        if (line.startsWith(QStringLiteral("Name:"))) {
                            line.remove(0, 5);
                            // if we're using this name, surround with []s to indicate it's not a file
                            info.setName(QStringLiteral("[%1]").arg(line.trimmed()));
                            break;
                        }
                    }
                    status.close();
                }
            }

            // Get the username.
            QFile status(processDir.absoluteFilePath(QStringLiteral("status")));
            if (status.open(QIODevice::ReadOnly)) {

                QByteArray contents = status.readAll();

                QTextStream in(&contents);

                while(!in.atEnd()) {
                    QString line = in.readLine();

                    if (line.startsWith(QStringLiteral("Uid:"))) {
                        //qDebug() << line;
                        //qDebug() << line.split(QRegExp("\\s+")).at(0);
                        //qDebug() << line.split(QRegExp("\\s+")).at(1);
                        //qDebug() << "";
                        info.setUsername(line.split(QRegExp("\\s+")).at(1));
                        break;
                    }
                }
                status.close();

                struct passwd* pw = getpwuid(info.username().toULong());
                if (pw) {
                    info.setUsername(pw->pw_name);
                }
            }

            // Get the command line
            QFile cmdline(processDir.absoluteFilePath(QStringLiteral("cmdline")));

            if (cmdline.open(QIODevice::ReadOnly)) {
                QByteArray contents = cmdline.readAll();

                int nullIdx = contents.indexOf('\0');

                if (nullIdx > 0) {

                    QString firstparam = QString::fromUtf8(contents.data(), nullIdx);

                    // if name is a truncated form of a filename, replace it
                    if (firstparam.endsWith(info.name()) && QFileInfo::exists(firstparam)) {
                        info.setName(QFileInfo(firstparam).completeBaseName());
                    }

                    // if we don't have a name, replace it but with []s
                    if (info.name().isEmpty()) {
                        info.setName(QStringLiteral("[%1]").arg(firstparam));
                    }

                    contents.replace('\0', ' ');
                }

                info.setCommandLine(QString::fromUtf8(contents).trimmed());

                cmdline.close();
            }

            // Add the process to the list.
            ret.push_back(info);
        }
    }

    return ret;
}

QProcessInfo::QProcessInfo() {
    _pid = 0;
}

uint32_t QProcessInfo::pid() const {
    return _pid;
}

void QProcessInfo::setPid(uint32_t pid) {
    _pid = pid;
}

const QString &QProcessInfo::username() const {
    return _username;
}

void QProcessInfo::setUsername(const QString &username) {
    _username = username;
}

const QString &QProcessInfo::name() const {
    return _name;
}

void QProcessInfo::setName(const QString &name) {
    _name = name;
}

const QString &QProcessInfo::commandLine() const {
    return _cmdLine;
}

void QProcessInfo::setCommandLine(const QString &cmd) {
    _cmdLine = cmd;
}

