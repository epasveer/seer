#pragma once

#include <QtCharts/QXYSeries>
#include <QtWidgets/QWidget>
#include "ui_SeerArrayVisualizerWidget.h"

class SeerArrayVisualizerWidget : public QWidget, protected Ui::SeerArrayVisualizerWidgetForm {

    Q_OBJECT

    public:
        explicit SeerArrayVisualizerWidget (QWidget* parent = 0);
       ~SeerArrayVisualizerWidget ();

        void                setAVariableName                     (const QString& name);
        QString             aVariableName                        () const;
        void                setAVariableAddress                  (const QString& address);
        QString             aVariableAddress                     () const;

        void                setBVariableName                     (const QString& name);
        QString             bVariableName                        () const;
        void                setBVariableAddress                  (const QString& address);
        QString             bVariableAddress                     () const;

    signals:
        void                evaluateVariableExpression          (int expressionid, QString expression);
        void                evaluateMemoryExpression            (int expressionid, QString address, int count);

    public slots:
        void                handleText                          (const QString& text);

    protected slots:
        void                handleaRefreshButton                ();
        void                handlebRefreshButton                ();
        void                handleaVariableNameLineEdit         ();
        void                handlebVariableNameLineEdit         ();
        void                handleaArrayDisplayFormatComboBox   (int index);
        void                handlebArrayDisplayFormatComboBox   (int index);
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
        int                 _aVariableId;
        int                 _bVariableId;
        int                 _aMemoryId;
        int                 _bMemoryId;
};

