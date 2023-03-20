#include "SeerKeySettings.h"
#include <QtCore/QList>

SeerKeySettings::SeerKeySettings () {
}

SeerKeySettings::SeerKeySettings (const SeerKeySettings& other) {

    *this = other;
}

SeerKeySettings::~SeerKeySettings () {
}

SeerKeySettings& SeerKeySettings::operator= (const SeerKeySettings& rhs) {

    _keys = rhs._keys;

    return *this;
}

QStringList SeerKeySettings::keys () const {

    QList<QString> keylist = _keys.keys();

    QStringList keys;

    for (int i=0; i<keylist.count(); i++) {
        keys.push_back(keylist[i]);
    }

    return keys;
}

bool SeerKeySettings::has (const QString& action) const {

    return _keys.contains(action);
}

SeerKeySetting SeerKeySettings::get (const QString& action) const {

    return _keys[action];
}

void SeerKeySettings::add (const QString& action, const SeerKeySetting& setting) {

    _keys[action] = setting;
}

int SeerKeySettings::count () const {

    return _keys.size();
}

SeerKeySettings SeerKeySettings::populate () {

    SeerKeySettings keySettings;

    keySettings.add("Run",              SeerKeySetting("Run",              QKeySequence::fromString("F1"),           "Run the program again. Do not break in \"main\"."));
    keySettings.add("Start",            SeerKeySetting("Start",            QKeySequence::fromString("F2"),           "Run the program again. Break in \"main\"."));
    keySettings.add("Continue",         SeerKeySetting("Continue",         QKeySequence::fromString("F8"),           "Continue execution of the program."));
    keySettings.add("Next",             SeerKeySetting("Next",             QKeySequence::fromString("F5"),           "Execute the next line. Step over functions."));
    keySettings.add("Step",             SeerKeySetting("Step",             QKeySequence::fromString("F6"),           "Execute the next line. Step into functions."));
    keySettings.add("Nexti",            SeerKeySetting("Nexti",            QKeySequence::fromString("Ctrl+F5"),      "Execute the next instruction. Step over functions."));
    keySettings.add("Stepi",            SeerKeySetting("Stepi",            QKeySequence::fromString("Ctrl+F6"),      "Execute the next instruction. Step into functions."));
    keySettings.add("Finish",           SeerKeySetting("Finish",           QKeySequence::fromString("F7"),           "Finish the current function."));
    keySettings.add("Interrupt",        SeerKeySetting("Interrupt",        QKeySequence::fromString("Ctrl+I"),       "Interrupt the executing program."));
    keySettings.add("Debug",            SeerKeySetting("Debug",            QKeySequence::fromString("Alt+D"),        "Open the debug dialog."));
    keySettings.add("Arguments",        SeerKeySetting("Arguments",        QKeySequence::fromString("Alt+A"),        "Open the argument dialog."));
    keySettings.add("Quit",             SeerKeySetting("Quit",             QKeySequence::fromString("Alt+Q"),        "Quit Seer."));
    keySettings.add("SearchText",       SeerKeySetting("SearchText",       QKeySequence::fromString("Ctrl+F"),       "Seach for text in the code editor."));
    keySettings.add("SearchTextNext",   SeerKeySetting("SearchTextNext",   QKeySequence::fromString("Ctrl+G"),       "Seach for next text in the code editor."));
    keySettings.add("SearchTextPrev",   SeerKeySetting("SearchTextPrev",   QKeySequence::fromString("Ctrl+Shift+G"), "Seach for previous text in the code editor."));
    keySettings.add("AlternateDir",     SeerKeySetting("AlternateDir",     QKeySequence::fromString("Ctrl+O"),       "Look for source file in an alternate directory."));

    return keySettings;
}

