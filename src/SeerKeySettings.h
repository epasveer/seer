#pragma once

#include <QtGui/QKeySequence>
#include <QtCore/QMap>
#include <QtCore/QStringList>
#include <QtCore/QString>

struct SeerKeySetting {

    SeerKeySetting(QString name, QKeySequence sequence, QString help) : _name(name), _sequence(sequence), _help(help) {}
    SeerKeySetting() {};

    QString         _name;
    QKeySequence    _sequence;
    QString         _help;
};


class SeerKeySettings {

    public:
        SeerKeySettings ();
        SeerKeySettings (const SeerKeySettings& other);
       ~SeerKeySettings ();

        SeerKeySettings& operator== (const SeerKeySettings& rhs);

        QStringList                             keys                () const;
        bool                                    has                 (const QString& name) const;
        SeerKeySetting                          get                 (const QString& name) const;
        void                                    add                 (const QString& name, const SeerKeySetting& setting);

        static SeerKeySettings                  populate            ();

    private:
        QMap<QString, SeerKeySetting>          _keys;
};

