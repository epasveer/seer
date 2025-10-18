// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "SeerHighlighterSettings.h"

SeerHighlighterSettings::SeerHighlighterSettings () {
}

SeerHighlighterSettings::SeerHighlighterSettings (const SeerHighlighterSettings& other) {

    *this = other;
}

SeerHighlighterSettings::~SeerHighlighterSettings () {
}

SeerHighlighterSettings& SeerHighlighterSettings::operator= (const SeerHighlighterSettings& rhs) {

    _keys               = rhs._keys;
    _formats            = rhs._formats;
    _cppSourceSuffixes  = rhs._cppSourceSuffixes;
    _rustSourceSuffixes = rhs._rustSourceSuffixes;
    _odinSourceSuffixes = rhs._odinSourceSuffixes;

    return *this;
}

QStringList SeerHighlighterSettings::keys () const {

    return _keys;
}

bool SeerHighlighterSettings::has (const QString& name) const {

    int i = _keys.indexOf(name);

    if (i < 0) {
        return false;
    }

    return true;
}

QTextCharFormat SeerHighlighterSettings::get (const QString& name) const {

    int i = _keys.indexOf(name);

    if (i < 0) {
        return QTextCharFormat();
    }

    return _formats[i];
}

void SeerHighlighterSettings::add (const QString& name, QTextCharFormat& format) {

    int i = _keys.indexOf(name);

    if (i < 0) {
        _keys.append(name);
        _formats.append(format);

        return;
    }

    _formats[i] = format;
}

int SeerHighlighterSettings::count () const {

    return _keys.size();
}

void SeerHighlighterSettings::setCppSourceSuffixes (const QString& suffixes) {

    _cppSourceSuffixes = suffixes;
}

void SeerHighlighterSettings::setOdinSourceSuffixes (const QString& suffixes) {

    _odinSourceSuffixes = suffixes;
}

void SeerHighlighterSettings::setRustSourceSuffixes (const QString& suffixes) {

    _rustSourceSuffixes = suffixes;
}

const QString& SeerHighlighterSettings::cppSourceSuffixes () {

    return _cppSourceSuffixes;
}

const QString& SeerHighlighterSettings::odinSourceSuffixes () {

    return _odinSourceSuffixes;
}

const QString& SeerHighlighterSettings::rustSourceSuffixes () {

    return _rustSourceSuffixes;
}

QStringList SeerHighlighterSettings::themeNames() {

    QStringList names;

    names << "light" << "dark";

    return names;
}

SeerHighlighterSettings SeerHighlighterSettings::populate (const QString& themeName) {

    if (themeName == "light") {
        return SeerHighlighterSettings::populate_light();
    }else if (themeName == "dark") {
        return SeerHighlighterSettings::populate_dark();
    }

    return SeerHighlighterSettings::populate_light();
}

SeerHighlighterSettings SeerHighlighterSettings::populate_light () {

    SeerHighlighterSettings languageSettings;

    QTextCharFormat f;

    f = QTextCharFormat();
    f.setFontWeight(QFont::Normal);
    f.setFontItalic(false);
    f.setForeground(QColor("#000000"));
    f.setBackground(QColor("#ffffff"));
    languageSettings.add("Text", f);

    f = QTextCharFormat();
    f.setFontWeight(QFont::Normal);
    f.setFontItalic(false);
    f.setForeground(QColor("#c0c0c0"));
    f.setBackground(QColor("#ffffff"));
    languageSettings.add("Assembly Text", f);

    f = QTextCharFormat();
    f.setFontWeight(QFont::Normal);
    f.setFontItalic(false);
    f.setForeground(QColor("#000000"));
    f.setBackground(QColor("#c0c0c0"));
    languageSettings.add("Margin", f);

    f = QTextCharFormat();
    f.setFontWeight(QFont::Normal);
    f.setFontItalic(false);
    f.setForeground(QColor("#000000"));
    f.setBackground(QColor("#ffff99"));
    languageSettings.add("Current Line", f);

    f = QTextCharFormat();
    f.setFontWeight(QFont::Normal);
    f.setFontItalic(false);
    f.setForeground(QColor("#000000"));
    f.setBackground(QColor("#c0c0c0"));
    languageSettings.add("Calling Line", f);

    f = QTextCharFormat();
    f.setFontWeight(QFont::Normal);
    f.setFontItalic(false);
    f.setForeground(QColor("#000000"));
    f.setBackground(QColor("#c0c0c0"));
    languageSettings.add("Match", f);

    f = QTextCharFormat();
    f.setFontWeight(QFont::Bold);
    f.setFontItalic(false);
    f.setForeground(QColor("#800080"));
    f.setBackground(QColor("#ffffff"));
    languageSettings.add("Class", f);

    f = QTextCharFormat();
    f.setFontWeight(QFont::Normal);
    f.setFontItalic(false);
    f.setForeground(QColor("#008000"));
    f.setBackground(QColor("#ffffff"));
    languageSettings.add("Quotation", f);

    f = QTextCharFormat();
    f.setFontWeight(QFont::Normal);
    f.setFontItalic(true);
    f.setForeground(QColor("#0000ff"));
    f.setBackground(QColor("#ffffff"));
    languageSettings.add("Function", f);

    f = QTextCharFormat();
    f.setFontWeight(QFont::Normal);
    f.setFontItalic(false);
    f.setForeground(QColor("#ff0000"));
    f.setBackground(QColor("#ffffff"));
    languageSettings.add("Comment", f);

    f = QTextCharFormat();
    f.setFontWeight(QFont::Normal);
    f.setFontItalic(false);
    f.setForeground(QColor("#ff0000"));
    f.setBackground(QColor("#ffffff"));
    languageSettings.add("Multiline Comment", f);

    f = QTextCharFormat();
    f.setFontWeight(QFont::Bold);
    f.setFontItalic(false);
    f.setForeground(QColor("#000080"));
    f.setBackground(QColor("#ffffff"));
    languageSettings.add("Keyword", f);

    languageSettings.setCppSourceSuffixes(".c|.C|.cpp|.CPP|.cxx|.CXX|.h|.H|.hpp|.hxx|.Hxx|.HXX");
    languageSettings.setOdinSourceSuffixes(".odin");
    languageSettings.setRustSourceSuffixes(".rs");

    return languageSettings;
}

SeerHighlighterSettings SeerHighlighterSettings::populate_dark () {

    SeerHighlighterSettings languageSettings;

    QTextCharFormat f;

    f = QTextCharFormat();
    f.setFontWeight(QFont::Normal);
    f.setFontItalic(false);
    f.setForeground(QColor("#b2b2b2"));
    f.setBackground(QColor("#232629"));
    languageSettings.add("Text", f);

    f = QTextCharFormat();
    f.setFontWeight(QFont::Normal);
    f.setFontItalic(false);
    f.setForeground(QColor("#32ae48"));
    f.setBackground(QColor("#232629"));
    languageSettings.add("Assembly Text", f);

    f = QTextCharFormat();
    f.setFontWeight(QFont::Normal);
    f.setFontItalic(false);
    f.setForeground(QColor("#7c7f81"));
    f.setBackground(QColor("#31363b"));
    languageSettings.add("Margin", f);

    f = QTextCharFormat();
    f.setFontWeight(QFont::Normal);
    f.setFontItalic(false);
    f.setForeground(QColor("#c7fa54"));
    f.setBackground(QColor("#8ea82f"));
    languageSettings.add("Current Line", f);

    f = QTextCharFormat();
    f.setFontWeight(QFont::Normal);
    f.setFontItalic(false);
    f.setForeground(QColor("#c7fa54"));
    f.setBackground(QColor("#737373"));
    languageSettings.add("Calling Line", f);

    f = QTextCharFormat();
    f.setFontWeight(QFont::Normal);
    f.setFontItalic(false);
    f.setForeground(QColor("#000000"));
    f.setBackground(QColor("#737373"));
    languageSettings.add("Match", f);

    f = QTextCharFormat();
    f.setFontWeight(QFont::Bold);
    f.setFontItalic(false);
    f.setForeground(QColor("#32ae48"));
    f.setBackground(QColor("#232629"));
    languageSettings.add("Class", f);

    f = QTextCharFormat();
    f.setFontWeight(QFont::Normal);
    f.setFontItalic(false);
    f.setForeground(QColor("#f453de"));
    f.setBackground(QColor("#232629"));
    languageSettings.add("Quotation", f);

    f = QTextCharFormat();
    f.setFontWeight(QFont::Normal);
    f.setFontItalic(true);
    f.setForeground(QColor("#736a59"));
    f.setBackground(QColor("#232629"));
    languageSettings.add("Function", f);

    f = QTextCharFormat();
    f.setFontWeight(QFont::Normal);
    f.setFontItalic(false);
    f.setForeground(QColor("#52f8f8"));
    f.setBackground(QColor("#232629"));
    languageSettings.add("Comment", f);

    f = QTextCharFormat();
    f.setFontWeight(QFont::Normal);
    f.setFontItalic(false);
    f.setForeground(QColor("#52f8f8"));
    f.setBackground(QColor("#232629"));
    languageSettings.add("Multiline Comment", f);

    f = QTextCharFormat();
    f.setFontWeight(QFont::Bold);
    f.setFontItalic(false);
    f.setForeground(QColor("#d9f743"));
    f.setBackground(QColor("#232629"));
    languageSettings.add("Keyword", f);

    languageSettings.setCppSourceSuffixes(".c|.C|.cpp|.CPP|.cxx|.CXX|.h|.H|.hpp|.hxx|.Hxx|.HXX");
    languageSettings.setOdinSourceSuffixes(".odin");
    languageSettings.setRustSourceSuffixes(".rs");

    return languageSettings;
}

