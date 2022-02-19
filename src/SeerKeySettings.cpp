#include "SeerKeySettings.h"
#include <QtCore/QList>

SeerKeySettings::SeerKeySettings () {
}

SeerKeySettings::SeerKeySettings (const SeerKeySettings& other) {

    *this = other;
}

SeerKeySettings::~SeerKeySettings () {
}

SeerKeySettings& SeerKeySettings::operator== (const SeerKeySettings& rhs) {

    _keys = rhs._keys;
}

QStringList SeerKeySettings::keys () const {

    QList<QString> keylist = _keys.keys();

    QStringList keys;

    for (int i=0; i<keylist.count(); i++) {
        keys.push_back(keylist[i]);
    }

    return keys;
}

bool SeerKeySettings::has (const QString& name) const {

    return _keys.contains(name);
}

SeerKeySetting SeerKeySettings::get (const QString& name) const {

    return _keys[name];
}

void SeerKeySettings::add (const QString& name, const SeerKeySetting& setting) {

    _keys[name] = setting;
}

SeerKeySettings SeerKeySettings::populate () {

    SeerKeySettings keySettings;

    keySettings.add("Run",          SeerKeySetting("Run",          QKeySequence::fromString("F1"),     "Run the program again. Do not break in main()."));
    keySettings.add("Start",        SeerKeySetting("Start",        QKeySequence::fromString("F1"),     "Run the program again. Break in main()."));
    keySettings.add("Continue",     SeerKeySetting("Continue",     QKeySequence::fromString("F8"),     "Continue execution of the program."));
    keySettings.add("Next",         SeerKeySetting("Next",         QKeySequence::fromString("F5"),     "Execute the next line. Step over functions."));
    keySettings.add("Step",         SeerKeySetting("Step",         QKeySequence::fromString("F6"),     "Execute the next line. Step into functions."));
    keySettings.add("Finish",       SeerKeySetting("Finish",       QKeySequence::fromString("F7"),     "Finish the current function."));
    keySettings.add("Debug",        SeerKeySetting("Debug",        QKeySequence::fromString("Alt+D"),  "Open the debug dialog."));
    keySettings.add("TextFind",     SeerKeySetting("TextFind",     QKeySequence::fromString("Ctrl+F"), "Seach for text in the code editor."));
    keySettings.add("AlternateDir", SeerKeySetting("AlternateDir", QKeySequence::fromString("Ctrl+O"), "Look for source file in an alternate directory."));

    return keySettings;
}

