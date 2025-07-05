#pragma once

#include <QtWidgets/QWidget>
#include <QtGui/QColor>
#include <QtCore/QString>
#include "ui_SeerStackDumpBrowserWidget.h"

class SeerStackDumpBrowserWidget : public QWidget, protected Ui::SeerStackDumpBrowserWidgetForm {

    Q_OBJECT

    public:
        explicit SeerStackDumpBrowserWidget (QWidget* parent = 0);
       ~SeerStackDumpBrowserWidget ();

        void                setStackPointerExpression   (const QString& expression);
        QString             stackPointerExpression      () const;

        void                setBytesBeforeSP            (int nbytes);
        int                 bytesBeforeSP               () const;

        void                setBytesAfterSP             (int nbytes);
        int                 bytesAfterSP                () const;

        void                setAsciiBytes               (int nbytes);
        int                 asciiBytes                  () const;

        void                setStackPointerColor        (const QColor& color);
        QColor              stackPointerColor           () const;

    public slots:
        void                handleText                  (const QString& text);
        void                handleStoppingPointReached  ();
        void                handleSessionTerminated     ();
        void                refresh                     ();

    protected slots:
        void                handleFormatComboBox        (const QString& text);
        void                handleVisualizerToolButton  ();
        void                handlePreferencesToolButton ();
        void                handleCellDoubleClicked     (int row, int col);

    signals:
        void                refreshStackPointer         (int id, QString expression);
        void                refreshStackDump            (int id, QString address, int offset, int count);
        void                addMemoryVisualizer         (QString expression);

    protected:
        void                showEvent                   (QShowEvent* event);
        void                readSettings                ();
        void                writeSettings               ();

    private:
        void                _populateTable              (QString address, QString contents);
        int                 _spExpressionId;
        int                 _dumpExpressionId;
        QString             _stackPointerExpression;
        int                 _bytesBeforeSP;
        int                 _bytesAfterSP;
        int                 _asciiBytes;
        QColor              _stackPointerColor;
};

