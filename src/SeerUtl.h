#pragma once
#include "QStringPair.h"
#include <QtCore/QString>
#include <QtCore/QStringList>
#include <QtCore/QMap>
#include <QtCore/Qt>

class Seer {

    public:

    static QString                     version             ();

    static QString                     filterEscapes       (const QString& str);
    static QStringList                 filterEscapes       (const QStringList& strings);
    static QString                     expandTabs          (const QString& str, int tabwidth, bool morph);
    static QString                     expandEnv           (const QString& str, bool* ok = nullptr);
    static QStringList                 parse               (const QString& str, const QString& search, QChar startBracket, QChar endBracket, bool includeSearch);
    static QString                     parseFirst          (const QString& str, const QString& search, QChar startBracket, QChar endBracket, bool includeSearch);
    static QString                     parseFirst          (const QString& str, const QString& search, bool includeSearch);
    static bool                        hasBookends         (const QString& str, QChar startBracket, QChar endBracket);
    static QString                     filterBookends      (const QString& str, QChar startBracket, QChar endBracket);
    static QStringList                 filterBookends      (const QStringList& strings, QChar startBracket, QChar endBracket);
    static QStringList                 parseCommaList      (const QString& str);
    static QStringList                 parseCommaList      (const QString& str, QChar startBracket, QChar endBracket);
    static QMap<QString,QString>       createKeyValueMap   (const QStringList& list, QChar separator);
    static QStringPair                 parseNameValue      (const QString& str, QChar separator);
    static QString                     quoteChars          (const QString& str, const QString& chars);
    static QStringList                 quoteChars          (const QStringList& strings, const QString& chars);
    static QString                     varObjParent        (const QString& str);
    static bool                        matchesWildcard     (const QStringList& regexpatterns, const QString& string);
    static bool                        hasWildcards        (const QString& str);
    static QString                     elideText           (const QString& str, Qt::TextElideMode mode, int length);
    static QStringList                 split               (const QString& str);
    static QString                     unescape            (const QString& str);
    static int                         createID            ();
    static unsigned char               ebcdicToAscii       (unsigned char byte);
    static unsigned char               ucharToAscii        (unsigned char byte);
    static QString                     ucharToHex          (const QVector<quint8>& bytes, int from, int count);
    static QString                     ucharToOctal        (const QVector<quint8>& bytes, int from, int count);
    static QString                     ucharToAscii        (const QVector<quint8>& bytes, int from, int count);
    static QString                     ucharToUShort       (const QVector<quint8>& bytes, int from, int count);
    static QString                     ucharToShort        (const QVector<quint8>& bytes, int from, int count);
    static QString                     ucharToUInt         (const QVector<quint8>& bytes, int from, int count);
    static QString                     ucharToInt          (const QVector<quint8>& bytes, int from, int count);
    static QString                     ucharToULong        (const QVector<quint8>& bytes, int from, int count);
    static QString                     ucharToLong         (const QVector<quint8>& bytes, int from, int count);
    static QString                     ucharToFloat        (const QVector<quint8>& bytes, int from, int count);
    static QString                     ucharToDouble       (const QVector<quint8>& bytes, int from, int count);
    static int                         typeBytes           (const QString& type);
    static bool                        readFile            (const QString& filename, QStringList& lines);
    static void                        printStackTrace     ();
};

