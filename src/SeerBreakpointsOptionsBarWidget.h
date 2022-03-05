#pragma once

#include <QtWidgets/QWidget>
#include "ui_SeerBreakpointsOptionsBarWidget.h"

class SeerBreakpointsOptionsBarWidget : public QWidget, protected Ui::SeerBreakpointsOptionsBarWidgetForm {

    Q_OBJECT

    public:
        explicit SeerBreakpointsOptionsBarWidget (QWidget* parent = 0);
       ~SeerBreakpointsOptionsBarWidget ();

        QToolButton*            breakpointsLoadToolButton       ();
        QToolButton*            breakpointsSaveToolButton       ();
};

