#include "../../src/SeerUtl.h"
#include <QtCore/QString>
#include <QtCore/QStringList>
#include <QtCore/QDebug>
#include <QtCore/QRegExp>
#include <iostream>

int main (int argc, char** argv) {

    Q_UNUSED(argc);
    Q_UNUSED(argv);

    //
    //
    //

  //QString test = "\"r0\",\"\",\"\",\"r4\"";
    QString test = "\"\",\"r0\",\"\",\"r4\"";
    QString newtest = Seer::filterEscapes(test);

    std::cout << test.toStdString() << std::endl;
    std::cout << newtest.toStdString() << std::endl;

    QStringList list = Seer::parse(test, "", '"', '"', false);

    for ( const auto& name : list  ) {
        std::cout << name.toStdString() << std::endl;
    }

    //
    //
    //

    QString text = "1^done,value=\"0\"";

    if (text.contains(QRegExp("^([0-9]+)\\^"))) {
        qDebug() << text << "matches";
    }else{
        qDebug() << text << "doesn't match";
    }

    //
    //
    //

    QString text2 = "1^done,value=\"\\\"Hello, World!\\\"\"";

    qDebug() << text2;

    QStringList list2 = Seer::parse(text2, "value=", '"', '"', false);

    for ( const auto& name : list2  ) {
        std::cout << name.toStdString() << std::endl;
    }

    //
    //
    //

    QString text3 = "name = \"Pasveer, Ernie\", age = 60, salary = 0.25, location = {city = \"Houston\", state = \"Texas\", zip = 77063}";

    qDebug() << text3;

    QStringList list3 = Seer::parseCommaList(text3, '{', '}');

    for ( const auto& name : list3  ) {

        QStringPair pair = Seer::parseNameValue(name, '=');

        std::cout << name.toStdString() << std::endl;
        std::cout << pair.first.toStdString() << std::endl;
        std::cout << pair.second.toStdString() << std::endl;
        std::cout << std::endl;
    }

    //
    //
    //

    QStringList lines;

    bool f = Seer::readFile("helloutl.cpp", lines);

    if (f == false) {
        std::cout << "== Can't open file for Seer::readFile()" << std::endl;
    }

    std::cout << "== Lines from Seer::readFile()" << std::endl;

    for ( const auto& line : lines  ) {
        std::cout << line.toStdString();
    }

    std::cout << "== Done." << std::endl;
    std::cout << std::endl;
    std::cout << std::endl;
    std::cout << std::endl;

    //
    //
    //

    QString envtext("Home dir is '$HOME' - My path is '${PATH}'");
    bool    envflag = false;

    QString text4 = Seer::expandEnv(envtext, &envflag);

    std::cout << "ENV text            : '" << envtext.toStdString() << "'" << std::endl;
    std::cout << "ENV expansion status: "  << (envflag ? "worked" : "failed") << std::endl;
    std::cout << "ENV expansion text  : '" << text4.toStdString() << std::endl;
    std::cout << std::endl;

    envtext = QString("Good '$HOME' - Other bad '${XXXX}'");

    text4 = Seer::expandEnv(envtext, &envflag);

    std::cout << "ENV text            : '" << envtext.toStdString() << "'" << std::endl;
    std::cout << "ENV expansion status: "  << (envflag ? "worked" : "failed") << std::endl;
    std::cout << "ENV expansion text  : '" << text4.toStdString() << std::endl;
    std::cout << std::endl;

    return 0;
}

