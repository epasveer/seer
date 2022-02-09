#include "SeerVariableManagerWidget.h"
#include <QtWidgets/QToolButton>
#include <QtGui/QIcon>
#include <QtCore/QDebug>

SeerVariableManagerWidget::SeerVariableManagerWidget (QWidget* parent) : QWidget(parent) {

    // Initialize private data

    // Setup UI
    setupUi(this);

    // Setup the widgets
    tabWidget->setMovable(true);
    tabWidget->setTabsClosable(false);

    _variableLoggerBrowserWidget  = new SeerVariableLoggerBrowserWidget(this);
    _variableTrackerBrowserWidget = new SeerVariableTrackerBrowserWidget(this);
    _registerValuesBrowserWidget  = new SeerRegisterValuesBrowserWidget(this);

    tabWidget->addTab(_variableLoggerBrowserWidget,  "Logger");
    tabWidget->addTab(_variableTrackerBrowserWidget, "Tracker");
    tabWidget->addTab(_registerValuesBrowserWidget,  "Registers");

    QToolButton* refreshToolButton = new QToolButton(tabWidget);
    refreshToolButton->setIcon(QIcon(":/seer/resources/RelaxLightIcons/view-refresh.svg"));
    refreshToolButton->setToolTip("Refresh the variable/register information.");
    tabWidget->setCornerWidget(refreshToolButton, Qt::TopRightCorner);

    // Connect things.
    QObject::connect(refreshToolButton, &QToolButton::clicked,     this,  &SeerVariableManagerWidget::handleRefreshToolButtonClicked);
}

SeerVariableManagerWidget::~SeerVariableManagerWidget () {
}

SeerVariableTrackerBrowserWidget* SeerVariableManagerWidget::variableTrackerBrowserWidget () {
    return _variableTrackerBrowserWidget;
}

SeerVariableLoggerBrowserWidget* SeerVariableManagerWidget::variableLoggerBrowserWidget () {
    return _variableLoggerBrowserWidget;
}

SeerRegisterValuesBrowserWidget* SeerVariableManagerWidget::registerValuesBrowserWidget () {
    return _registerValuesBrowserWidget;
}

void SeerVariableManagerWidget::handleRefreshToolButtonClicked () {

    variableTrackerBrowserWidget()->refresh();
    registerValuesBrowserWidget()->refresh();
}

