#include "SeerBreakpointsOptionsBarWidget.h"

SeerBreakpointsOptionsBarWidget::SeerBreakpointsOptionsBarWidget (QWidget* parent) : QWidget(parent) {

    // Construct the UI.
    setupUi(this);
}

SeerBreakpointsOptionsBarWidget::~SeerBreakpointsOptionsBarWidget () {
}

QToolButton* SeerBreakpointsOptionsBarWidget::breakpointsLoadToolButton () {

    return breakpointsLoadTB;
}

QToolButton* SeerBreakpointsOptionsBarWidget::breakpointsSaveToolButton () {

    return breakpointsSaveTB;
}

