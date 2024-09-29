#pragma once

#include <QtWidgets/QWidget>
#include <QtCore/QString>
#include "ui_SeerStackDumpBrowserWidget.h"

class SeerStackDumpBrowserWidget : public QWidget, protected Ui::SeerStackDumpBrowserWidgetForm {

    Q_OBJECT

    public:
        explicit SeerStackDumpBrowserWidget (QWidget* parent = 0);
       ~SeerStackDumpBrowserWidget ();

    public slots:
        void                handleText                  (const QString& text);
        void                handleStoppingPointReached  ();
        void                refresh                     ();

    protected slots:
        void                handleFormatComboBox        (const QString& text);
        void                handleVisualizerToolButton  ();

    signals:
        void                refreshStackPointer         (int id, QString expression);
        void                refreshStackDump            (int id, QString address, int offset, int count);
        void                addMemoryVisualize          (QString expression);

    protected:
        void                showEvent                   (QShowEvent* event);

    private:
        void                _populateTable              (QString address, QString contents);
        int                 _spExpressionId;
        int                 _dumpExpressionId;
};

