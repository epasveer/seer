#pragma once

#include <QtCharts/QXYSeries>
#include <QtWidgets/QWidget>
#include "ui_SeerMatrixVisualizerWidget.h"

class SeerMatrixVisualizerWidget : public QWidget, protected Ui::SeerMatrixVisualizerWidgetForm {

    Q_OBJECT

    public:
        explicit SeerMatrixVisualizerWidget (QWidget* parent = 0);
       ~SeerMatrixVisualizerWidget ();

        void                setVariableName                         (const QString& name);
        QString             variableName                            () const;
        void                setVariableAddress                      (const QString& address);
        QString             variableAddress                         () const;
        void                setVariableRows                         (const QString& rows);
        QString             variableRows                            () const;
        void                setVariableColumns                      (const QString& columns);
        QString             variableColumns                         () const;
        void                setVariableOffset                       (const QString& offset);
        QString             variableOffset                          () const;
        void                setVariableStride                       (const QString& stride);
        QString             variableStride                          () const;

    signals:
        void                evaluateVariableExpression              (int expressionid, QString expression);
        void                evaluateMemoryExpression                (int expressionid, QString address, int count);

    public slots:
        void                handleText                              (const QString& text);

    protected slots:
        void                handleRefreshButton                     ();
        void                handleVariableNameLineEdit              ();
        void                handleElementRowsLineEdit               ();
        void                handleElementColumnsLineEdit            ();
        void                handleElementOffsetLineEdit             ();
        void                handleElementStrideLineEdit             ();
        void                handleMatrixDisplayFormatComboBox       (int index);
        void                handleDataChanged                       ();

    protected:
        void                writeSettings                           ();
        void                readSettings                            ();
        void                resizeEvent                             (QResizeEvent* event);

    private:
        int                 _variableId;
        int                 _memoryId;
        int                 _rowsId;
        int                 _columnsId;
        int                 _offsetId;
        int                 _strideId;
};

