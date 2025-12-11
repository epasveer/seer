#include <QtWidgets/QApplication>
#include <QtCore/QFile>
#include <QtCore/QString>
#include <QtCore/QDebug>
#include <QtCore/QTextStream>
#include <QtCore/QString>
#include <QtCore/QDebug>

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

    enum STATE{
        START       = 0,
        SCANNING    = 1,
        FOUND       = 2,
        IGNORE      = 3,
    };

    QStringList list;
    int         index        = 0;
    int         start        = 0;
    int         end          = 0;
    int         previousComma= 0;
    STATE       state        = START;

    while (index < str.length()) {

        // Handle "="
        if (str[index] == '=') {
            if (state == SCANNING)               // looking for 2nd '='
            {
                // state = FOUND;
                end = previousComma;
                QString field = str.mid(start, end-start);
                list.append(field.trimmed());
                start = previousComma + 1;
            }
            if (state == START)               // looking for 2nd '='
            {
                state = SCANNING;

            }
        }

        if (str[index] == startBracket)
        {
            state = IGNORE;
        }

        if (str[index] == endBracket)
        {
            state = SCANNING;
        }

        if (str[index] == ',' && state != IGNORE) {
            previousComma = index;
        }

        // The last
        if (index == str.length() - 1)
        {
            QString field = str.mid(start, index);
            list.append(field.trimmed());
        }
        index++;
    }
    return list;
}

void readLinesFromFile(const QString &filePath) {

    QFile file(filePath);

    // Open file in read-only mode
    if (!file.open(QIODevice::ReadOnly | QIODevice::Text)) {
        qDebug() << "Cannot open file:" << file.errorString();
        return;
    }

    QTextStream in(&file);

    // Read line by line
    while (!in.atEnd()) {
        qDebug() << "========================================================================================================\n";

        QStringList list;
        QString line = in.readLine();
        qDebug() << "New string: " << line << "\n";
        list = parseCommaList(line, '{', '}');
        qDebug() << "Output: " << list;
    }

    file.close();
}

int main(int argc, char *argv[]) {

    QApplication app(argc, argv);

    // "/home/quangnm/Desktop/testcase.txt"
    // "C:/path/to/file.txt"

    readLinesFromFile(argv[1]);

    return app.exec();
}

