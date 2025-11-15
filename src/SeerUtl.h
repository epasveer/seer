// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once
#include "QStringPair.h"
#include <QtCore/QString>
#include <QtCore/QStringList>
#include <QtCore/QMap>
#include <QtCore/Qt>

namespace Seer {

    QString                     version             ();

    QString                     filterBareNewLines  (const QString& str);
    QString                     filterEscapes       (const QString& str);
    QStringList                 filterEscapes       (const QStringList& strings);
    QString                     expandTabs          (const QString& str, int tabwidth, bool morph);
    QString                     expandEnv           (const QString& str, bool* ok = nullptr);
    QStringList                 parse               (const QString& str, const QString& search, QChar startBracket, QChar endBracket, bool includeSearch);
    QString                     parseFirst          (const QString& str, const QString& search, QChar startBracket, QChar endBracket, bool includeSearch);
    QString                     parseFirst          (const QString& str, const QString& search, bool includeSearch);
    bool                        hasBookends         (const QString& str, QChar startBracket, QChar endBracket);
    QString                     filterBookends      (const QString& str, QChar startBracket, QChar endBracket);
    QStringList                 filterBookends      (const QStringList& strings, QChar startBracket, QChar endBracket);
    QStringList                 parseCommaList      (const QString& str);
    QStringList                 parseCommaList      (const QString& str, QChar startBracket, QChar endBracket);
    QStringList                 parseArray          (const QString& parentName, const QString& str);
    QMap<QString,QString>       createKeyValueMap   (const QStringList& list, QChar separator);
    QStringPair                 parseNameValue      (const QString& str, QChar separator);
    QString                     quoteChars          (const QString& str, const QString& chars);
    QStringList                 quoteChars          (const QStringList& strings, const QString& chars);
    QString                     varObjParent        (const QString& str);
    bool                        matchesWildcard     (const QStringList& regexpatterns, const QString& string);
    bool                        hasWildcards        (const QString& str);
    QString                     elideText           (const QString& str, Qt::TextElideMode mode, int length);
    QStringList                 split               (const QString& str);
    QString                     unescape            (const QString& str);
    int                         createID            ();
    unsigned char               ebcdicToAscii       (unsigned char byte);
    unsigned char               ucharToAscii        (unsigned char byte);
    QString                     ucharToHex          (const QVector<quint8>& bytes, int from, int count);
    QString                     ucharToOctal        (const QVector<quint8>& bytes, int from, int count);
    QString                     ucharToAscii        (const QVector<quint8>& bytes, int from, int count);
    QString                     ucharToUShort       (const QVector<quint8>& bytes, int from, int count);
    QString                     ucharToShort        (const QVector<quint8>& bytes, int from, int count);
    QString                     ucharToUInt         (const QVector<quint8>& bytes, int from, int count);
    QString                     ucharToInt          (const QVector<quint8>& bytes, int from, int count);
    QString                     ucharToULong        (const QVector<quint8>& bytes, int from, int count);
    QString                     ucharToLong         (const QVector<quint8>& bytes, int from, int count);
    QString                     ucharToFloat        (const QVector<quint8>& bytes, int from, int count);
    QString                     ucharToDouble       (const QVector<quint8>& bytes, int from, int count);
    int                         typeBytes           (const QString& type);
    bool                        readFile            (const QString& filename, QStringList& lines);
    void                        printStackTrace     ();
}

