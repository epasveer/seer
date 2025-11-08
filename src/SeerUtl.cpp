// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "SeerUtl.h"
#include <QtCore/QFile>
#include <QtCore/QString>
#include <QtCore/QDebug>
#include <QtCore/QRegularExpression>
#include <QtCore/QRegularExpressionMatch>
#include <QtCore/QByteArray>

// Comment out for now. I don't want to include boost because
// that would impact people try to compile Seer. Qt6 offer
// a 'stacktrace' function. OpenSuse may be slow in adoption.
// For now, comment it out and use when needed.
// #include <boost/stacktrace.hpp>

#include <iostream>
#include <mutex>

//
// Increment this with every release on GitHub.
// See scripts/change_versionnumber
//
#define SEER_VERSION "2.7beta"

namespace Seer {

    QString version () {
        return SEER_VERSION + QString(" (Qt") + QT_VERSION_STR + ")";
    }

    QString filterBareNewLines (const QString& str) {

        // Remove bare (not inside quotes) '\n' substrings.
        // Maybe extra spaces after '\n'.
        // "{\n  B = 1,\n  C = 0,\n  D = \"asd\",\n  E = std::vector<bool> of length 1, capacity 64 = {true}\n}"
        // "{B = 1, C = 0, D = \"asd\", E = std::vector<bool> of length 1, capacity 64 = {true}}"

        QString   tmp;
        qsizetype anchor=0;
        qsizetype curr=0;
        qsizetype end=str.length();

        // Loop through string a character at a time.
        while (curr < end) {

            // Found a '\n' substring. Deal with it.
            if (str.mid(curr,2) == "\\n") {

                // Append the good bits before the '\n'.
                tmp.append(str.mid(anchor,curr-anchor));
                curr += 2;

                // Skip trailing spaces, if any, after '\n'.
                while (curr < end) {
                    if (str.at(curr) == ' ') {
                        curr++;
                    }else{
                        break;
                    }
                }

                anchor = curr;

            // Other character. Skip it.
            }else{
                curr++;
            }
        }

        // Anything left to deal with?
        if (curr > anchor) {
            tmp.append(str.mid(anchor,curr-anchor));
            anchor = curr;
        }

        return tmp;
    }

    QString filterEscapes (const QString& str) {

        // Remove one level of '\'.
        // value="\"'Treasure' by Lucillius\\n\\n\\tbut theirs.\\n\""

        QString tmp;
        bool    escaped = false;

        for (int i=0; i<str.length(); i++) {
            if (str[i] == '\\') {
                if (escaped == false) {
                    escaped = true;
                    continue;
                }else{
                    escaped = false;
                    tmp.append(str[i]);
                }
            }else{
                escaped = false;
                tmp.append(str[i]);
            }
        }

        return tmp;
    }

    QStringList filterEscapes (const QStringList& strings) {

        QStringList list;

        // For a list of strings, quote certain characters.
        for (int i=0; i<strings.size(); i++) {
            list.append(Seer::filterEscapes(strings[i]));
        }

        // Return the new list.
        return list;
    }

    QString expandTabs (const QString& str, int tabsize, bool morph) {

        QString work = str;

        if (morph) {
            work.replace("\\t", "\t");
        }

        QString result;

        int pos = 0;

        for (int i=0; i<work.size(); i++) {

            QChar c = work.at(i);

            if (c == '\t') {
                // append the spaces here.
                int nspaces = tabsize - pos % tabsize;

                for (int i=0; i<nspaces; i++) {
                    result.append(' ');
                }

                pos = 0;

            }else{
                result.append(c);
                pos = (c == '\n') ? 0 : pos + 1;
            }
        }

        return result;
    }

    QString expandEnv (const QString& str, bool* ok) {

        QRegularExpression env_re1("\\$\\{[A-Za-z0-9_]+\\}");      // ${PATH}
        QRegularExpression env_re2("\\$[a-zA-Z0-9_]+");            // $PATH

        QString r = str;
        bool    f = true;

        while (true) {

            QRegularExpressionMatch match = env_re1.match(r);

            if (match.hasMatch() == false) {
                break;
            }

            qsizetype i      = match.capturedStart();
            qsizetype len    = match.capturedLength();
            QString   capstr = match.captured();
            QString   envstr = capstr.mid(2,capstr.length()-3);

            QByteArray value(qgetenv(envstr.toLatin1().data()));

            if (value.size() > 0) {
                r.remove(i, len);
                r.insert(i, value);
            }else{
                f = false; // Not expanded.
                break;
            }
        }

        while (true) {

            QRegularExpressionMatch match = env_re2.match(r);

            if (match.hasMatch() == false) {
                break;
            }

            qsizetype i      = match.capturedStart();
            qsizetype len    = match.capturedLength();
            QString   capstr = match.captured();
            QString   envstr = capstr.mid(1,capstr.length()-1);

            QByteArray value(qgetenv(envstr.toLatin1().data()));

            if (value.size() > 0) {
                r.remove(i, len);
                r.insert(i, value);
            }else{
                f = false; // Not expanded.
                break;
            }
        }

        if (ok) {
            *ok = f;
        }

        return r;
    }

    QStringList parse (const QString& str, const QString& search, QChar startBracket, QChar endBracket, bool includeSearchString) {

        //
        // str          = "^done,stack-args=[frame={level=\"0\",args=[{name=\"message\",value=\"\\\"Hello, World!\\\"\"}]},frame={level=\"1\",args=[{name=\"argc\",value=\"1\"},{name=\"argv\",value=\"0x7fffffffd5b8\"}]}]"
        // search       = "frame="
        // startBracket = '{'
        // endBracket   = '}'
        //

        QStringList list;
        QString     searchWord = search + startBracket; // 'frame={'
        int         from       = 0;

        while (1) {

            // Look for the next occurance of the search word.
            int index = str.indexOf(searchWord,from);
            if (index < 0) { // If not found, we're done.
                break;
            }

            // Set things up to look for the matching end bracket.
            int  start        = index + search.size() + 1; // Positioned after 'frame={'
            int  end          = start + 1;
            int  bracketLevel = 1; // Start with one start bracket already encountered.

            for (end=start; end<str.size(); end++) { // Start is already 1 past the opening bracket.

                bool escaped = false;

                if (end-1 >= 0) {
                    if (str[end-1] == '\\') {
                        escaped = true;
                    }
                }

                if (startBracket != endBracket) { // Do this test if brackets are not the same character.
                    if (str[end] == startBracket && escaped == false) { // Encountered another start bracket, increment the level.
                        bracketLevel++;
                    }
                }

                if (str[end] == endBracket && escaped == false) { // Encountered an end bracket, decrement the level.
                    bracketLevel--;

                    if (bracketLevel == 0) { // If the level is 0, we're done.
                        break;
                    }
                }
            }

            // Are things valid?
            if (bracketLevel != 0) { // No matching bracket.
                break;
            }

            if (end > str.size()) { // The end is past the end of the string.
                break;
            }

            // Build the string.
            QString tmp;

            if (includeSearchString) {
                tmp = search + startBracket + str.mid(start, end-start) + endBracket;
            }else{
                tmp = str.mid(start, end-start);
            }

            // Add it to the list.
            list.append(tmp);

            // Go back for more
            from = end + 1;
        }

        // Return the list.
        return list;
    }

    QString parseFirst (const QString& str, const QString& search, QChar startBracket, QChar endBracket, bool includeSearchString) {

        QStringList list = Seer::parse(str, search, startBracket, endBracket, includeSearchString);

        if (list.size() == 0) {
            return QString();
        }

        return list.front();
    }

    QString parseFirst (const QString& str, const QString& search, bool includeSearchString) {

        QString match;

        // Look for the next occurance of the search word.
        int index = str.indexOf(search,0);
        if (index < 0) { // If not found, we're done.
            return match;
        }

        // Set things up to look for the matching end bracket.
        int start = index + search.size() + 0; // Position after 'msg='
        int end   = str.size();

        if (includeSearchString) {
            match = search + str.mid(start, end-start);
        }else{
            match = str.mid(start, end-start);
        }

        return match;
    }


    bool hasBookends (const QString& str, QChar startBracket, QChar endBracket) {

        if (str.startsWith(startBracket) && str.endsWith(endBracket)) {
            return true;
        }

        return false;
    }


    QString filterBookends (const QString& str, QChar startBracket, QChar endBracket) {

        // If the string starts with and ends with the bracket characters, then return
        // the text between the brackets.
        if (str.startsWith(startBracket) && str.endsWith(endBracket)) {
            return str.mid(1,str.size()-2);
        }

        // Otherwise, return the orginal string.
        return str;
    }

    QStringList filterBookends (const QStringList& strings, QChar startBracket, QChar endBracket) {

        QStringList list;

        // For a list of strings, remove the bookends, if possible.
        for (int i=0; i<strings.size(); i++) {
            list.append(Seer::filterBookends(strings[i], startBracket, endBracket));
        }

        // Return the new list.
        return list;
    }

    QStringList parseCommaList (const QString& str) {

        //
        // number="2",type="breakpoint",disp="keep",enabled="y",addr="0x00000000004016cd",func="main(int, char**)",file="hellofibonacci.cpp",fullname="/nas/erniep/Development/seer/tests/hellofibonacci/hellofibonacci.cpp",line="34",thread-groups=["i1"],cond="$_streq(s.c_str(), "21")",times="0",original-location="hellofibonacci.cpp:34"
        //
        // returns...
        //
        // number="2"
        // type="breakpoint"
        // disp="keep"
        // enabled="y"
        // addr="0x00000000004016cd"
        // func="main(int char**)"
        // file="hellofibonacci.cpp"
        // fullname="/nas/erniep/Development/seer/tests/hellofibonacci/hellofibonacci.cpp"
        // line="34"
        // thread-groups=["i1"]
        // cond="$_streq(s.c_str() "21")"
        // times="0"
        // original-location="hellofibonacci.cpp:34"
        //

        QStringList list;
        int         index        = 0;
        int         state        = 0;
        int         start        = 0;
        int         end          = 0;
        bool        inquotes     = false;
        int         bracketlevel = 0;

        while (index < str.length()) {

            // Handle start of field.
            if (state == 0) {     // Start of field.
                start = index;
                end   = index;
                state = 1; // Look for end of field (a command or eol).

                continue;
            }

            // Handle end of field.
            if (state == 1) {

                // Handle """
                if (str[index] == '"') {
                    if (inquotes == false) {
                        inquotes = true;
                    }else{
                        inquotes = false;
                    }

                    index++; continue;
                }

                // Handle ","
                if (str[index] == ',') {
                    if (inquotes) {
                        index++; continue;
                    }

                    // Extract field, only if the bracket level is at zero.
                    // Otherwise, continue.
                    if (bracketlevel == 0) {
                        end = index;

                        QString field = str.mid(start, end-start);

                        list.append(field.trimmed());

                        state = 0; // Look for the next field.
                    }

                    index++; continue;
                }

                // Handle any other character.
                index++; continue;
            }

            qDebug() << "Bad state!";
            index++; continue;
        }

        // Handle last field, if any.
        if (state == 1) {
            end = index;

            QString field = str.mid(start, end-start);

            list.append(field.trimmed());
        }

        return list;
    }

    QStringList parseCommaList  (const QString& str, QChar startBracket, QChar endBracket) {

        // name = "Pasveer, Ernie", age = 60, salary = 0.25, location = {city = "Houston", state = "Texas", zip = 77063}
        //
        // name = "Pasveer, Ernie"
        // age = 60
        // salary = 0.25
        // location = {city = "Houston", state = "Texas", zip = 77063}
        //
        // special case: garbage value:
        // {id = 2, b = \"000hildTest 2000377177000000260324377377377177\", '000' <repeats 11 times>, \"hildTest 2000-", '0' <repeats 11 times>, "1", '0' <repeats 21 times>, "330324377377377177000000004", '0' <repeats 21 times>, "test\", '000' <repeats 21 times>, \"325377377\", child = {childId = -9864, childString = \"\"}}

        QStringList list;
        int         index        = 0;
        int         state        = 0;
        int         start        = 0;
        int         end          = 0;
        bool        inquotes     = false;
        int         bracketlevel = 0;

        while (index < str.length()) {

            // Handle start of field.
            if (state == 0) {     // Start of field.
                start = index;
                end   = index;
                state = 1; // Look for end of field (a command or eol).

                continue;
            }

            // Handle end of field.
            if (state == 1) {

                // Handle "{"
                if (str[index] == startBracket) {
                    if (inquotes == false) {
                        bracketlevel++;
                    }

                    index++; continue;
                }

                // Handle "}"
                if (str[index] == endBracket) {
                    if (inquotes == false) {
                        bracketlevel--;
                        if (bracketlevel < 0) {
                            qDebug() << "BracketLevel is less than 0!";
                        }
                    }

                    index++; continue;
                }

                // Handle """
                if (str[index] == '"') {
                    if (inquotes == false) {
                        inquotes = true;
                    }else{
                        inquotes = false;
                    }

                    index++; continue;
                }

                // Handle ","
                if (str[index] == ',') {
                    if (inquotes) {
                        index++; continue;
                    }

                    // Extract field, only if the bracket level is at zero.
                    // Otherwise, continue.
                    if (bracketlevel == 0) {
                        end = index;

                        QString field = str.mid(start, end-start);

                        list.append(field.trimmed());

                        state = 0; // Look for the next field.
                    }

                    index++; continue;
                }

                // Handle any other character.
                index++; continue;
            }

            qDebug() << "Bad state!";
            index++; continue;
        }

        // Handle last field, if any.
        if (state == 1) {
            end = index;

            QString field = str.mid(start, end-start);

            list.append(field.trimmed());
        }

        return list;
    }

    QMap<QString,QString> createKeyValueMap (const QStringList& list, QChar separator) {

        QMap<QString,QString> map;

        for (const auto& i : list) {
            QStringPair pair = parseNameValue(i, separator);

            map[pair.first] = pair.second;
        }

        return map;
    }

    //
    //
    //

    QStringPair parseNameValue (const QString& str, QChar separator) {

        // name = "Pasveer, Ernie"
        //
        // pair.first  = name
        // pair.second = "Pasveer, Ernie"
        //

        QStringPair pair;
        int         index        = 0;
        int         state        = 0;
        int         start        = 0;
        int         end          = 0;
        bool        inquotes     = false;

        while (index < str.length()) {

            // Handle start of field.
            if (state == 0) {     // Start of field.
                start = index;
                end   = index;
                state = 1; // Look for end of field (a command or eol).

                continue;
            }

            // Handle end of field.
            if (state == 1) {

                // Handle """
                if (str[index] == '"') {
                    if (inquotes == false) {
                        inquotes = true;
                    }else{
                        inquotes = false;
                    }

                    index++; continue;
                }

                // Handle "=" separator.
                if (str[index] == separator) {

                    if (inquotes) {
                        index++; continue;
                    }

                    // Extract the two fields.  index is at the "=".
                    end = index;

                    QString one = str.mid(start, end-start);
                    QString two = str.mid(end+1);

                    pair = QStringPair(one.trimmed(),two.trimmed());

                    state = 0; // Look for the next field.

                    break;
                }

                // Handle any other character.
                index++; continue;
            }

            qDebug() << "Bad state!";
            index++; continue;
        }

        // Handle if no "=" separator.
        if (state == 1) {
            pair = QStringPair(str.trimmed(),QString());
        }

        return pair;
    }

    //
    // Quote certain characters in a string.
    //
    //       "hello"  =>  \"hello\"
    //
    QString quoteChars (const QString& str, const QString& chars) {

        QString string;

        for (int i=0; i<str.size(); i++) {

            QChar c = str[i];

            if (chars.contains(c)) {
                string.append('\\');
                string.append(c);
            }else{
                string.append(c);
            }
        }

        return string;
    }

    QStringList quoteChars (const QStringList& strings, const QString& chars) {

        QStringList list;

        // For a list of strings, quote certain characters.
        for (int i=0; i<strings.size(); i++) {
            list.append(Seer::quoteChars(strings[i], chars));
        }

        // Return the new list.
        return list;
    }


    //
    //
    //

    QString varObjParent (const QString& str) {

        //
        // input:  "seer4.public.location"
        // output: "seer4.public"
        //
        // input:  "seer4"
        // output: ""
        //

        // Break the string into parts, delimited by a '.'.
        QStringList parts = str.split('.');

        // Remove the last part.
        if (parts.isEmpty() == false) {
            parts.removeLast();
        }

        // Return a string that has the parts rejoined.
        return parts.join('.');
    }

    //
    //
    //

    bool matchesWildcard (const QStringList& patterns, const QString& string) {

        foreach (auto pattern, patterns) {

#if QT_VERSION >= 0x060000

            QRegularExpression re = QRegularExpression::fromWildcard(pattern, Qt::CaseInsensitive, QRegularExpression::UnanchoredWildcardConversion);

#else
            // With Qt5, 'wildcardToRegularExpression' needs a "/*" at the start of 'pattern'
            // if 'string' start with a "/".
            //if (string[0] == '/') {
            //    if (pattern[0] != '/') {
            //        pattern = "/*/" + pattern;
            //    }
            //}

            // This pattern needs to be converted from a glob to a regex.
            // All '*' are replaced with '.*'
            // The pattern is terminated with a '$', if it doesn't have one.
            pattern.replace("*", ".*");

            //if (string[0] != '/') {
            //    if (pattern.back() != '$') {
            //        pattern += '$';
            //    }
            //}

            // qDebug() << pattern << string;

            // QRegularExpression re = QRegularExpression(QRegularExpression::wildcardToRegularExpression(pattern));
            QRegularExpression re = QRegularExpression(pattern);
#endif

            if (re.match(string).hasMatch()) {
                return true;
            }
        }

        return false;
    }

    bool hasWildcards (const QString& str) {
        return (str.indexOf('*') >= 0) || (str.indexOf('?') >= 0);
    }

    QString elideText (const QString& str, Qt::TextElideMode mode, int length) {

        QString leftElide("... ");
        QString middleElide(" ... ");
        QString rightElide(" ...");

        // The string is fine. Just return it.
        if (str.length() <= length) {
            return str;
        }

        // The string is too long but don't add elide.
        if (mode == Qt::ElideNone) {
            return str.left(length);
        }

        // The string is too long. Add elilde on the left.
        if (mode == Qt::ElideLeft) {
            return leftElide + str.right(length);
        }

        // The string is too long. Add elilde on the right.
        if (mode == Qt::ElideRight) {
            return str.left(length) + rightElide;
        }

        // The string is too long. Add elilde in the middle.
        if (mode == Qt::ElideRight) {
            int halve = length / 2;
            return str.left(halve) + middleElide + str.right(halve);
        }

        // Bad mode. Just return the string.
        return str;
    }

    // Split a string on words. Double "quoted strings"
    // act as one word.
    QStringList split (const QString& str) {

        QRegularExpression re("[^\\s\"']+|\"([^\"]*)\"|'([^']*)'");
        QStringList        list;

        QRegularExpressionMatchIterator i = re.globalMatch(str);
        while (i.hasNext()) {
            QRegularExpressionMatch match = i.next();

            QString word;
            if (match.captured(2) != "") {
                word = match.captured(2);
            }else if (match.captured(1) != "") {
                word = match.captured(1);
            }else if (match.captured(0) != "") {
                word = match.captured(0);
            }

            if (word != "") {
                list << word;
            }
        }

        return list;
    }

    //
    //
    //

    static int        Next_ID = 1;
    static std::mutex ID_mutex;

    int createID () {

         std::lock_guard<std::mutex> guard(ID_mutex);

         int id = Next_ID;

         Next_ID++;

         return id;
    }

    unsigned char ebcdicToAscii (unsigned char byte) {

        static const unsigned char ebcdicToAsciiTable[256] = {
             46,  46,  46,  46,  46,  46,  46,  46, // 0
             46,  46,  46,  46,  46,  46,  46,  46, // 8
             46,  46,  46,  46,  46,  46,  46,  46, // 16
             46,  46,  46,  46,  46,  46,  46,  46, // 24
             46,  46,  46,  46,  46,  46,  46,  46, // 32
             46,  46,  46,  46,  46,  46,  46,  46, // 40
             46,  46,  46,  46,  46,  46,  46,  46, // 48
             46,  46,  46,  46,  46,  46,  46,  46, // 56
             32,  46,  46,  46,  46,  46,  46,  46, // 64
             46,  46,  91,  46,  60,  40,  43,  33, // 72
             38,  46,  46,  46,  46,  46,  46,  46, // 80
             46,  46,  33,  36,  42,  41,  59,  94, // 88
             45,  47,  46,  46,  46,  46,  46,  46, // 96
             46,  46, 124,  44,  37,  95,  62,  63, // 104
             46,  46,  46,  46,  46,  46,  46,  46, // 112
             46,  96,  58,  35,  64,  39,  61,  34, // 120
             46,  97,  98,  99, 100, 101, 102, 103, // 128
            104, 105,  46,  46,  46,  46,  46,  46, // 136
             46, 106, 107, 108, 109, 110, 111, 112, // 144
            113, 114,  46,  46,  46,  46,  46,  46, // 152
             46, 126, 115, 116, 117, 118, 119, 120, // 160
            121, 122,  46,  46,  46,  46,  46,  46, // 168
             46,  46,  46,  46,  46,  46,  46,  46, // 176
             46,  46,  46,  46,  46,  46,  46,  46, // 184
            123,  65,  66,  67,  68,  69,  70,  71, // 192
             72,  73,  46,  46,  46,  46,  46,  46, // 200
            125,  74,  75,  76,  77,  78,  79,  80, // 208
             81,  82,  46,  46,  46,  46,  46,  46, // 216
             92,  46,  83,  84,  85,  86,  87,  88, // 224
             89,  90,  46,  46,  46,  46,  46,  46, // 232
             48,  49,  50,  51,  52,  53,  54,  55, // 240
             56,  57,  46,  46,  46,  46,  46,  46  // 248
        };

        return ebcdicToAsciiTable[byte];
    }

    unsigned char ucharToAscii (unsigned char byte) {

        static const unsigned char ucharToAsciiTable[256] = {
             46,  46,  46,  46,  46,  46,  46,  46, // 0
             46,  46,  46,  46,  46,  46,  46,  46, // 8
             46,  46,  46,  46,  46,  46,  46,  46, // 16
             46,  46,  46,  46,  46,  46,  46,  46, // 24
             32,  33,  34,  35,  36,  37,  38,  39, // 32
             40,  41,  42,  43,  44,  45,  46,  47, // 40
             48,  49,  50,  51,  52,  53,  54,  55, // 48
             56,  57,  58,  59,  60,  61,  62,  63, // 56
             64,  65,  66,  67,  68,  69,  70,  71, // 64
             72,  73,  74,  75,  76,  77,  78,  79, // 72
             80,  81,  82,  83,  84,  85,  86,  87, // 80
             88,  89,  90,  91,  92,  93,  94,  95, // 88
             96,  97,  98,  99, 100, 101, 102, 103, // 96
            104, 105, 106, 107, 108, 109, 110, 111, // 104
            112, 113, 114, 115, 116, 117, 118, 119, // 112
            120, 121, 122, 123, 124, 125, 126,  46, // 120
             46,  46,  46,  46,  46,  46,  46,  46, // 128
             46,  46,  46,  46,  46,  46,  46,  46, // 136
             46,  46,  46,  46,  46,  46,  46,  46, // 144
             46,  46,  46,  46,  46,  46,  46,  46, // 152
             46,  46,  46,  46,  46,  46,  46,  46, // 160
             46,  46,  46,  46,  46,  46,  46,  46, // 168
             46,  46,  46,  46,  46,  46,  46,  46, // 176
             46,  46,  46,  46,  46,  46,  46,  46, // 184
             46,  46,  46,  46,  46,  46,  46,  46, // 192
             46,  46,  46,  46,  46,  46,  46,  46, // 200
             46,  46,  46,  46,  46,  46,  46,  46, // 208
             46,  46,  46,  46,  46,  46,  46,  46, // 216
             46,  46,  46,  46,  46,  46,  46,  46, // 224
             46,  46,  46,  46,  46,  46,  46,  46, // 232
             46,  46,  46,  46,  46,  46,  46,  46, // 240
             46,  46,  46,  46,  46,  46,  46,  46  // 248
        };

        return ucharToAsciiTable[byte];
    }

    QString ucharToHex (const QVector<quint8>& bytes, int from, int count) {

        QString str;

        str.reserve(count*2+4);

        for (int i=from; i<from+count; i++) {
            if (i >= bytes.size()) {
                break;
            }

            quint8  decimal = bytes[i];
            QString hex     = QString("%1").arg(decimal, 2, 16, QLatin1Char('0'));

            str += hex;
        }

        return str;
    }

    QString ucharToOctal (const QVector<quint8>& bytes, int from, int count) {

        QString str;

        str.reserve(count*3+4);

        for (int i=from; i<from+count; i++) {
            if (i >= bytes.size()) {
                break;
            }

            quint8  decimal = bytes[i];
            QString octal   = QString("%1").arg(decimal, 3, 8, QLatin1Char('0'));

            str += octal;
        }

        return str;
    }

    QString ucharToAscii (const QVector<quint8>& bytes, int from, int count) {

        QString str;

        str.reserve(count);

        for (int i=from; i<from+count; i++) {
            if (i >= bytes.size()) {
                break;
            }

            quint8  decimal = bytes[i];
            QString ascii   = QChar(Seer::ucharToAscii(decimal));

            str += ascii;
        }

        return str;
    }

    QString ucharToUShort (const QVector<quint8>& bytes, int from, int count) {

        QString str;

        for (int i=0; i<count; i++) {
            if (from+1 >= bytes.size()) {
                break;
            }

            quint16 decimal = *(quint16*)(bytes.data()+from);
            QString val     = QString("%1").arg(decimal);

            if (i != 0) {
                str += " ";
            }

            str  += val;
            from += sizeof(quint16);
        }

        return str;
    }

    QString ucharToShort (const QVector<quint8>& bytes, int from, int count) {

        QString str;

        for (int i=0; i<count; i++) {
            if (from+1 >= bytes.size()) {
                break;
            }

            qint16  decimal = *(qint16*)(bytes.data()+from);
            QString val     = QString("%1").arg(decimal);

            if (i != 0) {
                str += " ";
            }

            str  += val;
            from += sizeof(qint16);
        }

        return str;
    }

    QString ucharToUInt (const QVector<quint8>& bytes, int from, int count) {

        QString str;

        for (int i=0; i<count; i++) {
            if (from+3 >= bytes.size()) {
                break;
            }

            quint32 decimal = *(quint32*)(bytes.data()+from);
            QString val     = QString("%1").arg(decimal);

            if (i != 0) {
                str += " ";
            }

            str  += val;
            from += sizeof(quint32);
        }

        return str;
    }

    QString ucharToInt (const QVector<quint8>& bytes, int from, int count) {

        QString str;

        for (int i=0; i<count; i++) {
            if (from+3 >= bytes.size()) {
                break;
            }

            qint32  decimal = *(qint32*)(bytes.data()+from);
            QString val     = QString("%1").arg(decimal);

            if (i != 0) {
                str += " ";
            }

            str  += val;
            from += sizeof(qint32);
        }

        return str;
    }

    QString ucharToULong (const QVector<quint8>& bytes, int from, int count) {

        QString str;

        for (int i=0; i<count; i++) {
            if (from+7 >= bytes.size()) {
                break;
            }

            quint64 decimal = *(quint64*)(bytes.data()+from);
            QString val     = QString("%1").arg(decimal);

            if (i != 0) {
                str += " ";
            }

            str  += val;
            from += sizeof(quint64);
        }

        return str;
    }

    QString ucharToLong (const QVector<quint8>& bytes, int from, int count) {

        QString str;

        for (int i=0; i<count; i++) {
            if (from+7 >= bytes.size()) {
                break;
            }

            qint64  decimal = *(qint64*)(bytes.data()+from);
            QString val     = QString("%1").arg(decimal);

            if (i != 0) {
                str += " ";
            }

            str  += val;
            from += sizeof(qint64);
        }

        return str;
    }

    QString ucharToFloat (const QVector<quint8>& bytes, int from, int count) {

        QString str;

        for (int i=0; i<count; i++) {
            if (from+3 >= bytes.size()) {
                break;
            }

            float    real = *(float*)(bytes.data()+from);
            QString  val  = QString("%1").arg(real);

            if (i != 0) {
                str += " ";
            }

            str  += val;
            from += sizeof(float);
        }

        return str;
    }

    QString ucharToDouble (const QVector<quint8>& bytes, int from, int count) {

        QString str;

        for (int i=0; i<count; i++) {
            if (from+7 >= bytes.size()) {
                break;
            }

            double   real = *(double*)(bytes.data()+from);
            QString  val  = QString("%1").arg(real);

            if (i != 0) {
                str += " ";
            }

            str  += val;
            from += sizeof(double);
        }

        return str;
    }

    int typeBytes (const QString& type) {

        if (type == "int8" || type == "uint8") {
            return 1;
        }else if (type == "int16" || type == "uint16") {
            return 2;
        }else if (type == "int32" || type == "uint32") {
            return 4;
        }else if (type == "int64" || type == "uint64") {
            return 8;
        }else if (type == "float32") {
            return 4;
        }else if (type == "float64") {
            return 8;
        }else{
            qWarning() << "Bad data type:" << type;
            return 0;
        }
    }

    bool readFile (const QString& filename, QStringList& lines) {

        // Empty the list
        lines = QStringList();

        // Open the file.
        QFile file(filename);

        if (file.open(QIODevice::ReadOnly) == false) {
            qDebug() << "Can't open:" << filename;
            return false;
        }

        // Read the file line-by-line and build up the string list.
        while (file.atEnd() == false) {

            QString line = file.readLine();
            line.chop(1);
            lines.append(line);
        }

        file.close();

        // All good.
        return true;
    }

    QString unescape (const QString& str) {

        QString result;
        bool    quoted = false;

        // Maybe reserve `str.length()` bytes in result, so that it will not allocate during `push_back()`
        result.reserve(str.length());

        // Loop through each character in 'str'. Look for a '\'.
        for (int i=0; i<str.length(); i++) {

            // Found a '\'.
            if (str[i] == QChar('\\')) {
                // Can we look at the following character?
                if (i+1 >= str.length()) {
                    // Nope, just add it.
                    result.push_back(str[i]);
                    break;
                }

                // If the following character is another '\', skip the first one.
                // This removes one level of '\'.
                if (str[i+1] == QChar('\\')) {
                    continue;
                }

                // Treat the following character as an escaped character.
                // Unescape it. (Is that even a word?)
                switch(str[i+1].unicode()) {
                    // We don't want to unescape things that are in double quotes.
                    // So keep track of that. (Not the more fool proof implementation).
                    case QChar('\"').unicode():
                        result.push_back(QChar('\"'));
                        if (quoted) {
                            quoted = false;
                        }else{
                            quoted = true;
                        }
                        break;
                    case QChar('n').unicode():
                        if (quoted == false) {
                            result.push_back(QChar('\n'));
                        }else{
                            result.push_back(QChar('\\'));
                            result.push_back(str[i+1]);
                        }
                        break;
                    case QChar('\'').unicode():
                        if (quoted == false) {
                            result.push_back(QChar('\''));
                        }else{
                            result.push_back(QChar('\\'));
                            result.push_back(str[i+1]);
                        }
                        break;
                    case QChar('\\').unicode():
                        if (quoted == false) {
                            result.push_back(QChar('\\'));
                        }else{
                            result.push_back(QChar('\\'));
                            result.push_back(str[i+1]);
                        }
                        break;
                    case QChar('t').unicode():
                        if (quoted == false) {
                            result.push_back(QChar('\t'));
                        }else{
                            result.push_back(QChar('\\'));
                            result.push_back(str[i+1]);
                        }
                        break;
                        // and so on for \a, \b, \e, \f, \r, \t, \v
                        // maybe handle octal, hexadecimal ASCII and Unicode forms, but probably in the more distant future
                        // ...
                    default:
                        // unknown escape sequence — do not escape it — I think it is reasonable default
                        result.push_back(QChar('\\'));
                        result.push_back(str[i+1]);
                        break;
                }
                i++;

            // Normal character. Just add it.
            }else{
                result.push_back(str[i]);
            }
        }

        return result;
    }

    void printStackTrace () {
        // Capture and print the stack trace using Boost.Stacktrace.
        // See comments at top.
        // std::cout << "Stack trace:\n" << boost::stacktrace::stacktrace() << std::endl;
    }
}

