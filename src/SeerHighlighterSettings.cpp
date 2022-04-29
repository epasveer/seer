#include "SeerHighlighterSettings.h"
#include <QtCore/QList>

SeerHighlighterSettings::SeerHighlighterSettings () {
}

SeerHighlighterSettings::SeerHighlighterSettings (const SeerHighlighterSettings& other) {

    *this = other;
}

SeerHighlighterSettings::~SeerHighlighterSettings () {
}

SeerHighlighterSettings& SeerHighlighterSettings::operator= (const SeerHighlighterSettings& rhs) {

    _formats = rhs._formats;

    return *this;
}

QStringList SeerHighlighterSettings::keys () const {

    QList<QString> keylist = _formats.keys();

    QStringList keys;

    for (int i=0; i<keylist.count(); i++) {
        keys.push_back(keylist[i]);
    }

    return keys;
}

bool SeerHighlighterSettings::has (const QString& name) const {

    return _formats.contains(name);
}

QTextCharFormat SeerHighlighterSettings::get (const QString& name) const {

    return _formats[name];
}

void SeerHighlighterSettings::add (const QString& name, QTextCharFormat& format) {

    _formats[name] = format;
}

int SeerHighlighterSettings::count () const {

    return _formats.size();
}

SeerHighlighterSettings SeerHighlighterSettings::populateForCPP () {

    SeerHighlighterSettings cppSettings;


    QTextCharFormat f;

    f.setFontWeight(QFont::Normal); f.setFontItalic(false); f.setForeground(Qt::black); f.setBackground(Qt::white);
    cppSettings.add("Text", f);
    f = QTextCharFormat();

    f.setFontWeight(QFont::Normal); f.setFontItalic(false); f.setForeground(Qt::black); f.setBackground(Qt::lightGray);
    cppSettings.add("Margin", f);
    f = QTextCharFormat();

    f.setFontWeight(QFont::Bold); f.setFontItalic(false); f.setForeground(Qt::darkMagenta); f.setBackground(Qt::white);
    cppSettings.add("Class", f);
    f = QTextCharFormat();

    f.setFontWeight(QFont::Normal); f.setFontItalic(false); f.setForeground(Qt::darkGreen); f.setBackground(Qt::white);
    cppSettings.add("Quotation", f);
    f = QTextCharFormat();

    f.setFontWeight(QFont::Normal); f.setFontItalic(true); f.setForeground(Qt::blue); f.setBackground(Qt::white);
    cppSettings.add("Function", f);
    f = QTextCharFormat();

    f.setFontWeight(QFont::Normal); f.setFontItalic(false); f.setForeground(Qt::red); f.setBackground(Qt::white);
    cppSettings.add("Comment", f);
    f = QTextCharFormat();

    f.setFontWeight(QFont::Normal); f.setFontItalic(false); f.setForeground(Qt::red); f.setBackground(Qt::white);
    cppSettings.add("Multiline Comment", f);
    f = QTextCharFormat();

    f.setFontWeight(QFont::Bold); f.setFontItalic(false); f.setForeground(Qt::darkBlue); f.setBackground(Qt::white);
    cppSettings.add("Keyword", f);
    f = QTextCharFormat();

    f.setFontWeight(QFont::Normal); f.setFontItalic(false); f.setForeground(Qt::black); f.setBackground(QColor(Qt::yellow).lighter(160));
    cppSettings.add("Current Line", f);
    f = QTextCharFormat();

    return cppSettings;
}

