// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

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
        void                setAVariableLength                   (const QString& length);
        QString             aVariableLength                      () const;
        void                setAVariableOffset                   (const QString& offset);
        QString             aVariableOffset                      () const;
        void                setAVariableStride                   (const QString& stride);
        QString             aVariableStride                      () const;

        void                setBVariableName                     (const QString& name);
        QString             bVariableName                        () const;
        void                setBVariableAddress                  (const QString& address);
        QString             bVariableAddress                     () const;
        void                setBVariableLength                   (const QString& length);
        QString             bVariableLength                      () const;
        void                setBVariableOffset                   (const QString& offset);
        QString             bVariableOffset                      () const;
        void                setBVariableStride                   (const QString& stride);
        QString             bVariableStride                      () const;

    signals:
        void                evaluateVariableExpression          (int expressionid, QString expression);
        void                evaluateMemoryExpression            (int expressionid, QString address, int count);

    public slots:
        void                handleText                          (const QString& text);

    protected slots:
        void                handleaRefreshButton                ();
        void                handlebRefreshButton                ();
        void                handleHelpButton                    ();
        void                handleaVariableNameLineEdit         ();
        void                handlebVariableNameLineEdit         ();
        void                handleaElementLengthLineEdit        ();
        void                handlebElementLengthLineEdit        ();
        void                handleaElementOffsetLineEdit        ();
        void                handlebElementOffsetLineEdit        ();
        void                handleaElementStrideLineEdit        ();
        void                handlebElementStrideLineEdit        ();
        void                handleaArrayDisplayFormatComboBox   (int index);
        void                handlebArrayDisplayFormatComboBox   (int index);
        void                handleaAxisComboBox                 (int index);
        void                handlebAxisComboBox                 (int index);
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
        void                createASeries                       ();
        void                createBSeries                       ();

        QXYSeries*          _aSeries;
        QXYSeries*          _bSeries;
        int                 _aVariableId;
        int                 _bVariableId;
        int                 _aMemoryId;
        int                 _aLengthId;
        int                 _aOffsetId;
        int                 _aStrideId;
        int                 _bMemoryId;
        int                 _bLengthId;
        int                 _bOffsetId;
        int                 _bStrideId;
};

