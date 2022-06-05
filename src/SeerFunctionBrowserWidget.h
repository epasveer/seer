#pragma once

#include <QtWidgets/QWidget>
#include <QtCore/QString>
#include "ui_SeerFunctionBrowserWidget.h"

class SeerFunctionBrowserWidget : public QWidget, protected Ui::SeerFunctionBrowserWidgetForm {

    Q_OBJECT

    public:
        explicit SeerFunctionBrowserWidget (QWidget* parent = 0);
       ~SeerFunctionBrowserWidget ();

    public slots:
        void                handleText                  (const QString& text);
        void                refresh                     ();

    protected slots:
        void                handleSearchLineEdit        ();
        void                handleItemDoubleClicked     (QTreeWidgetItem* item, int column);
        void                handleItemEntered           (QTreeWidgetItem* item, int column);

    signals:
        void                refreshFunctionList         (int id, const QString& functionRegex);
        void                selectedFile                (QString file, QString fullname, int lineno);

    protected:
    private:
        int                 _id;
};

