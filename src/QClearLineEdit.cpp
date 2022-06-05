#include "QClearLineEdit.h"
#include <QtCore/QDebug>

QClearLineEdit::QClearLineEdit (const QString& contents, QWidget* parent) : QLineEdit(contents, parent) {

    enableReturnPressedOnClear();
}

QClearLineEdit::QClearLineEdit (QWidget* parent) : QLineEdit(parent) {

    enableReturnPressedOnClear();
}

QClearLineEdit::~QClearLineEdit () {
}

void QClearLineEdit::enableReturnPressedOnClear() {

    for (int i=0; i <children().size(); i++) {

        QAction* myClearAction(qobject_cast<QAction*>(children().at(i)));

        if (myClearAction) {

            //qDebug() << myClearAction->objectName();

            // Look for only the QLineEdit clear action.
            // This name could change or be different.
            if (myClearAction->objectName() == "_q_qlineeditclearaction") {
                connect(myClearAction, &QAction::triggered, this, &QLineEdit::returnPressed, Qt::QueuedConnection);
            }
        }
    }
}

