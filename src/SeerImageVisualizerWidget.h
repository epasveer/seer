#pragma once

#include <QtWidgets/QWidget>
#include "ui_SeerImageVisualizerWidget.h"

class SeerImageVisualizerWidget : public QWidget, protected Ui::SeerImageVisualizerWidgetForm {

    Q_OBJECT

    public:
        explicit SeerImageVisualizerWidget (QWidget* parent = 0);
       ~SeerImageVisualizerWidget ();

        void                setVariableName                     (const QString& name);
        QString             variableName                        () const;
        void                setVariableAddress                  (const QString& address);
        QString             variableAddress                     () const;

    signals:
        void                evaluateVariableExpression          (int expressionid, QString expression);
        void                evaluateMemoryExpression            (int expressionid, QString address, int count);

    public slots:
        void                handleText                          (const QString& text);

    protected slots:
        void                handleRefreshButton                 ();
        void                handleHelpButton                    ();
        void                handleVariableNameLineEdit          ();
        void                handleFormatComboBox                (int index);
        void                handlePrintButton                   ();
        void                handleSaveButton                    ();
        void                handleCreateImage                   (const QByteArray& array);

    protected:
        void                writeSettings                       ();
        void                readSettings                        ();
        void                resizeEvent                         (QResizeEvent* event);

    private:
        int                 _variableId;
        int                 _memoryId;

        QString             _formatName;
        QImage::Format      _format;
        int                 _width;
        int                 _height;
        int                 _bytes;
};

