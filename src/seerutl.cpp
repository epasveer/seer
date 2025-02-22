#include "SeerUtl.h"
#include <QtCore/QDebug>

int main (int argc, char* argv[]) {

    Q_UNUSED(argc);
    Q_UNUSED(argv);

    QString text("Function\\n1");
    QString result;

    result = Seer::filterEscapes(text);

    qDebug().noquote() << text;
    qDebug().noquote() << result;

    return 1;
}

