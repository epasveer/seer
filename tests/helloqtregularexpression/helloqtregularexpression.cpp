#include <iostream>
#include <QtCore/QString>
#include <QtCore/QRegularExpression>
#include <QtCore/QDebug>

int main (int argc, char** argv) {

    if (argc != 3) {
        std::cout << "usage: " << argv[0] << " 'glob' 'test string'" << std::endl;
        return 1;
    }

    QString glob(argv[1]);
    QString test(argv[2]);

    qInfo() << "Glob string :" << glob;
    qInfo() << "Test string :" << test;

  //QString regex = QRegularExpression::wildcardToRegularExpression(glob);
    QString regex = glob;

    qInfo() << "Regex string:" << regex;

    // Test for match.
    QRegularExpression re = QRegularExpression(regex);

    if (re.match(test).hasMatch()) {
        qInfo() << "MATCH";
    }else{
        qInfo() << "NOMATCH";
    }

    return 0;
}

