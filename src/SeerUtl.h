#pragma once
#include "QStringPair.h"
#include <QtCore/QString>
#include <QtCore/QStringList>

namespace Seer {

    QString         version         ();

    QString         filterEscapes   (const QString& str);
    QString         expandTabs      (const QString& str, int tabwidth, bool morph);
    QString         expandEnv       (const QString& str, bool* ok = nullptr);
    QStringList     parse           (const QString& str, const QString& search, QChar startBracket, QChar endBracket, bool includeSearch);
    QString         parseFirst      (const QString& str, const QString& search, QChar startBracket, QChar endBracket, bool includeSearch);
    QString         parseFirst      (const QString& str, const QString& search, bool includeSearch);
    bool            hasBookends     (const QString& str, QChar startBracket, QChar endBracket);
    QString         filterBookends  (const QString& str, QChar startBracket, QChar endBracket);
    QStringList     filterBookends  (const QStringList& strings, QChar startBracket, QChar endBracket);
    QStringList     parseCommaList  (const QString& str, QChar startBracket, QChar endBracket);
    QStringPair     parseNameValue  (const QString& str, QChar separator);
    QString         quoteChars      (const QString& str, const QString& chars);
    QStringList     quoteChars      (const QStringList& strings, const QString& chars);
    QString         varObjParent    (const QString& str);
    bool            matchesWildcard (const QStringList& regexpatterns, const QString& string);

    int             createID        ();

    unsigned char   ebcdicToAscii   (unsigned char byte);
    unsigned char   ucharToAscii    (unsigned char byte);

    int             typeBytes       (const QString& type);

    bool            readFile        (const QString& filename, QStringList& lines);
}

