#include "SeerUtl.h"
#include <QtCore/QFile>
#include <QtCore/QString>
#include <QtCore/QDebug>
#include <QRegularExpression>
#include <mutex>

//
// Increment this with every release on GitHub.
// See scripts/change_versionnumber
//
#define SEER_VERSION "2.3beta"

namespace Seer {

    QString version () {
        return SEER_VERSION + QString(" (Qt") + QT_VERSION_STR + ")";
    }

    QString filterEscapes (const QString& str, bool handleCR) {

        QString tmp = str;

        tmp.replace("\\r",  "\r");
        tmp.replace("\\t",  "\t");
        tmp.replace("\\\"", "\"");

        if (handleCR) {
            tmp.replace("\\n",  "\n");
        }

        return tmp;
    }

    QStringList filterEscapes (const QStringList& strings, bool handleCR) {

        QStringList list;

        // For a list of strings, quote certain characters.
        for (int i=0; i<strings.size(); i++) {
            list.append(Seer::filterEscapes(strings[i], handleCR));
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

    QStringList parseCommaList  (const QString& str, QChar startBracket, QChar endBracket) {

        // name = "Pasveer, Ernie", age = 60, salary = 0.25, location = {city = "Houston", state = "Texas", zip = 77063}
        //
        // name = "Pasveer, Ernie"
        // age = 60
        // salary = 0.25
        // location = {city = "Houston", state = "Texas", zip = 77063}
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

    //
    //
    //

    QStringPair parseNameValue  (const QString& str, QChar separator) {

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
}

