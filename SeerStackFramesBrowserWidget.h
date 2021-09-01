#pragma once

#include <QtWidgets/QWidget>
#include <QtCore/QString>
#include "ui_SeerStackFramesBrowserWidget.h"

class SeerStackFramesBrowserWidget : public QWidget, protected Ui::SeerStackFramesBrowserWidgetForm {

    Q_OBJECT

    public:
        explicit SeerStackFramesBrowserWidget (QWidget* parent = 0);
       ~SeerStackFramesBrowserWidget ();

    public slots:
        void                handleText                  (const QString& text);
        void                handleItemDoubleClicked     (QTreeWidgetItem* item, int column);
        void                handleStoppingPointReached  ();
        void                refresh                     ();

    signals:
        void                refreshStackFrames          ();
        void                selectedFile                (QString file, QString fullname, int lineno);
        void                selectedFrame               (int frameno);

    protected:
        void                showEvent                   (QShowEvent* event);

    private:
        QString             _previousStackFrameText;
};

