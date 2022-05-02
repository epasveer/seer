#pragma once

#include <QtCharts/QXYSeries>
#include <QtWidgets/QWidget>
#include "ui_SeerArrayVisualizerWidget.h"

class SeerArrayVisualizerWidget : public QWidget, protected Ui::SeerArrayVisualizerWidgetForm {

    Q_OBJECT

    public:
        explicit SeerArrayVisualizerWidget (QWidget* parent = 0);
       ~SeerArrayVisualizerWidget ();

        void                setXVariableName                     (const QString& name);
        QString             xVariableName                        () const;
        void                setXVariableAddress                  (const QString& address);
        QString             xVariableAddress                     () const;

        void                setYVariableName                     (const QString& name);
        QString             yVariableName                        () const;
        void                setYVariableAddress                  (const QString& address);
        QString             yVariableAddress                     () const;

    signals:
        void                evaluateVariableExpression          (int expressionid, QString expression);
        void                evaluateMemoryExpression            (int expressionid, QString address, int count);

    public slots:
        void                handleText                          (const QString& text);

    protected slots:
        void                handlexRefreshButton                ();
        void                handleyRefreshButton                ();
        void                handlexVariableNameLineEdit         ();
        void                handleyVariableNameLineEdit         ();
        void                handlexArrayDisplayFormatComboBox   (int index);
        void                handleyArrayDisplayFormatComboBox   (int index);
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
        int                 _xVariableId;
        int                 _yVariableId;
        int                 _xMemoryId;
        int                 _yMemoryId;
};

