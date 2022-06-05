#pragma once

#include <QtWidgets/QWidget>
#include <QtCore/QString>
#include "ui_SeerVariableBrowserWidget.h"

class SeerVariableBrowserWidget : public QWidget, protected Ui::SeerVariableBrowserWidgetForm {

    Q_OBJECT

    public:
        explicit SeerVariableBrowserWidget (QWidget* parent = 0);
       ~SeerVariableBrowserWidget ();

    public slots:
        void                handleText                  (const QString& text);
        void                refresh                     ();

    protected slots:
        void                handleSearchLineEdit        ();
        void                handleItemDoubleClicked     (QTreeWidgetItem* item, int column);
        void                handleItemEntered           (QTreeWidgetItem* item, int column);

    signals:
        void                refreshVariableList         (int id, const QString& variableNameRegex, const QString& variableTypeRegex);
        void                selectedFile                (QString file, QString fullname, int lineno);

    protected:
    private:
        int                 _id;
};

