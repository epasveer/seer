#pragma once

#include <QtWidgets/QWidget>
#include <QtCore/QString>
#include "ui_SeerThreadFramesBrowserWidget.h"

class SeerThreadFramesBrowserWidget : public QWidget, protected Ui::SeerThreadFramesBrowserWidgetForm {

    Q_OBJECT

    public:
        explicit SeerThreadFramesBrowserWidget (QWidget* parent = 0);
       ~SeerThreadFramesBrowserWidget ();

    public slots:
        void                handleText                  (const QString& text);
        void                handleItemDoubleClicked     (QTreeWidgetItem* item, int column);
        void                handleStoppingPointReached  ();
        void                refresh                     ();

    signals:
        void                refreshThreadFrames         ();
        void                selectedFile                (QString file, QString fullname, int lineno);
        void                selectedFrame               (int frameno);

    protected:
        void                showEvent                   (QShowEvent* event);

    private:
};

