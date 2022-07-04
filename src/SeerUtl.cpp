#include "SeerUtl.h"
#include <QtCore/QDebug>
#include <mutex>

namespace Seer {

    QString version () {

        // Increment this with every release on GitHub.
        return "1.8beta";
    }

    QString filterEscapes (const QString& str) {

        QString tmp = str;

        tmp.replace("\\r",  "\r");
        tmp.replace("\\n",  "\n");
        tmp.replace("\\t",  "\t");
        tmp.replace("\\\"", "\"");

        return tmp;
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

    QString filterBookEnds (const QString& str, QChar startBracket, QChar endBracket) {

        // If the string starts with and ends with the bracket characters, then return
        // the text between the brackets.
        if (str.startsWith(startBracket) && str.endsWith(endBracket)) {
            return str.mid(1,str.size()-2);
        }

        // Otherwise, return the orginal string.
        return str;
    }

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
}

