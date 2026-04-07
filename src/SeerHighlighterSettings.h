// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include <QtGui/QTextCharFormat>
#include <QtCore/QList>
#include <QtCore/QStringList>
#include <QtCore/QString>

class SeerHighlighterSettings {

    public:
        SeerHighlighterSettings ();
        SeerHighlighterSettings (const SeerHighlighterSettings& other);
       ~SeerHighlighterSettings ();

        SeerHighlighterSettings& operator= (const SeerHighlighterSettings& rhs);

        QStringList                             keys                    () const;
        bool                                    has                     (const QString& name) const;
        QTextCharFormat                         get                     (const QString& name) const;
        void                                    add                     (const QString& name, QTextCharFormat& format);
        int                                     count                   () const;
        void                                    setCppSourceSuffixes    (const QString& suffixes);
        void                                    setOdinSourceSuffixes   (const QString& suffixes);
        void                                    setRustSourceSuffixes   (const QString& suffixes);
        const QString&                          cppSourceSuffixes       ();
        const QString&                          odinSourceSuffixes      ();
        const QString&                          rustSourceSuffixes      ();

        static QStringList                      themeNames              ();
        static SeerHighlighterSettings          populate                (const QString& themeName);
        static SeerHighlighterSettings          populate_light          ();
        static SeerHighlighterSettings          populate_dark           ();

    private:
        QList<QString>                          _keys;
        QList<QTextCharFormat>                  _formats;
        QString                                 _cppSourceSuffixes;
        QString                                 _odinSourceSuffixes;
        QString                                 _rustSourceSuffixes;
};

