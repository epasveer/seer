#pragma once

#include <QtWidgets/QWidget>
#include "ui_SeerMemoryVisualizerWidget.h"

class SeerMemoryVisualizerWidget : public QWidget, protected Ui::SeerMemoryVisualizerWidgetForm {

    Q_OBJECT

    public:
        explicit SeerMemoryVisualizerWidget (QWidget* parent = 0);
       ~SeerMemoryVisualizerWidget ();

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
        void                handleVariableNameLineEdit          ();
        void                handleMemoryDisplayFormatComboBox   (int index);
        void                handleCharDisplayFormatComboBox     (int index);
        void                handleColumnCountSpinBox            (int value);
        void                handlePrintButton                   ();
        void                handleSaveButton                    ();

    protected:
        void                writeSettings                       ();
        void                readSettings                        ();
        void                resizeEvent                         (QResizeEvent* event);

    private:
        int                 _variableId;
        int                 _memoryId;
};

