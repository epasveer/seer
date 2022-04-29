#pragma once

#include <QtGui/QTextCharFormat>
#include <QtCore/QMap>
#include <QtCore/QStringList>
#include <QtCore/QString>

class SeerHighlighterSettings {

    public:
        SeerHighlighterSettings ();
        SeerHighlighterSettings (const SeerHighlighterSettings& other);
       ~SeerHighlighterSettings ();

        SeerHighlighterSettings& operator= (const SeerHighlighterSettings& rhs);

        QStringList                             keys                () const;
        bool                                    has                 (const QString& name) const;
        QTextCharFormat                         get                 (const QString& name) const;
        void                                    add                 (const QString& name, QTextCharFormat& format);
        int                                     count               () const;

        static SeerHighlighterSettings          populateForCPP      ();

    private:
        QMap<QString, QTextCharFormat>          _formats;
};

