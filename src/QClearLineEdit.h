#pragma once

#include <QtWidgets/QLineEdit>
#include <QtWidgets/QAction>

class QClearLineEdit : public QLineEdit {

    public:
        QClearLineEdit (const QString& contents, QWidget* parent = nullptr);
        QClearLineEdit (QWidget* parent = nullptr);
       ~QClearLineEdit ();

        void            enableReturnPressedOnClear          ();

    public slots:
    protected:
    private:
};

