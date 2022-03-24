#pragma once

#include <QtCharts/QXYSeries>
#include <QtWidgets/QWidget>
#include "ui_SeerArrayVisualizerWidget.h"

class SeerArrayVisualizerWidget : public QWidget, protected Ui::SeerArrayVisualizerWidgetForm {

    Q_OBJECT

    public:
        explicit SeerArrayVisualizerWidget (QWidget* parent = 0);
       ~SeerArrayVisualizerWidget ();

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
        void                handleArrayDisplayFormatComboBox    (int index);
        void                handleDataChanged                   ();
        void                handleSplitterMoved                 (int pos, int index);
        void                handleSeriesHovered                 (const QPointF& point, bool state);
        void                handleTitleLineEdit                 ();
        void                handlePointsCheckBox                ();
        void                handleLabelsCheckBox                ();
        void                handleLineTypeButtonGroup           ();

    protected:
        void                writeSettings                       ();
        void                readSettings                        ();
        void                resizeEvent                         (QResizeEvent* event);

    private:
        QXYSeries*          _series;
        int                 _variableId;
        int                 _memoryId;
};

