#pragma once

#include "ui_SeerHelpPageWidget.h"

class SeerHelpPageWidget: public QWidget, protected Ui::SeerHelpPageWidgetForm {

    Q_OBJECT

    public:

        SeerHelpPageWidget(QWidget* parent = 0);
       ~SeerHelpPageWidget();

        void                        loadFile                            (const QString& filename);
        void                        loadText                            (const QString& text);

    signals:
    public slots:
    protected:
        void                        writeSettings                       ();
        void                        readSettings                        ();
        void                        resizeEvent                         (QResizeEvent* event);

    protected slots:
        void                        handlePrintToolButton               ();
        void                        handleOkPushButton                  ();

    private:
};

