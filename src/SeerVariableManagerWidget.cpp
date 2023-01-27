#include "SeerVariableManagerWidget.h"
#include "SeerHelpPageDialog.h"
#include "QHContainerWidget.h"
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

    QToolButton* helpToolButton = new QToolButton(tabWidget);
    helpToolButton->setIcon(QIcon(":/seer/resources/RelaxLightIcons/help-about.svg"));
    helpToolButton->setToolTip("Help on variable/register information.");

    QHContainerWidget* hcontainer = new QHContainerWidget(this);
    hcontainer->setSpacing(3);
    hcontainer->addWidget(refreshToolButton);
    hcontainer->addWidget(helpToolButton);

    tabWidget->setCornerWidget(hcontainer, Qt::TopRightCorner);

    // Connect things.
    QObject::connect(refreshToolButton, &QToolButton::clicked,     this,  &SeerVariableManagerWidget::handleRefreshToolButtonClicked);
    QObject::connect(helpToolButton,    &QToolButton::clicked,     this,  &SeerVariableManagerWidget::handleHelpToolButtonClicked);
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

void SeerVariableManagerWidget::handleHelpToolButtonClicked () {

    SeerHelpPageDialog* help = new SeerHelpPageDialog;
    help->loadFile(":/seer/resources/help/VariableRegisterInfoBrowser.md");
    help->show();
    help->raise();
}

