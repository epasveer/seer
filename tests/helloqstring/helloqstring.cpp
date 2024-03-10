#include <QtCore/QString>
#include <QtCore/QDebug>
#include <stdio.h>

int main (int argc, char* argv[]) {

    if (argc < 2) {
        printf("usage: %s arg1 ...\n", argv[0]);
        return 1;
    }

    for (int i=1; i<argc; i++) {

        QString str(argv[i]);

        qDebug() << str;
    }

    return 0;
}

