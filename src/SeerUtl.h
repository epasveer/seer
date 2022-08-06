#pragma once
#include "QStringPair.h"
#include <QtCore/QString>
#include <QtCore/QStringList>

namespace Seer {

    QString         version         ();

    QString         filterEscapes   (const QString& str);
    QStringList     parse           (const QString& str, const QString& search, QChar startBracket, QChar endBracket, bool includeSearch);
    QString         parseFirst      (const QString& str, const QString& search, QChar startBracket, QChar endBracket, bool includeSearch);
    QString         parseFirst      (const QString& str, const QString& search, bool includeSearch);
    QString         filterBookEnds  (const QString& str, QChar startBracket, QChar endBracket);
    QStringList     parseCommaList  (const QString& str, QChar startBracket, QChar endBracket);
    QStringPair     parseNameValue  (const QString& str, QChar separator);

    int             createID        ();

    unsigned char   ebcdicToAscii   (unsigned char byte);
    unsigned char   ucharToAscii    (unsigned char byte);

    int             typeBytes       (const QString& type);
}

