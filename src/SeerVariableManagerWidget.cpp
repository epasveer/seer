#include "SeerVariableManagerWidget.h"
#include "SeerHelpPageDialog.h"
#include "QHContainerWidget.h"
#include <QtWidgets/QToolButton>
#include <QtGui/QIcon>
#include <QtCore/QSettings>
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

    // Restore tab ordering.
    readSettings();

    // Connect things.
    QObject::connect(refreshToolButton,             &QToolButton::clicked,                          this,  &SeerVariableManagerWidget::handleRefreshToolButtonClicked);
    QObject::connect(helpToolButton,                &QToolButton::clicked,                          this,  &SeerVariableManagerWidget::handleHelpToolButtonClicked);
    QObject::connect(tabWidget->tabBar(),           &QTabBar::tabMoved,                             this,  &SeerVariableManagerWidget::handleTabMoved);
    QObject::connect(tabWidget->tabBar(),           &QTabBar::currentChanged,                       this,  &SeerVariableManagerWidget::handleTabChanged);
    QObject::connect(_variableLoggerBrowserWidget,  &SeerVariableLoggerBrowserWidget::raiseTab,     this,  &SeerVariableManagerWidget::handleRaiseLoggerTab);
    QObject::connect(_variableTrackerBrowserWidget, &SeerVariableTrackerBrowserWidget::raiseTab,    this,  &SeerVariableManagerWidget::handleRaiseTrackerTab);
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

void SeerVariableManagerWidget::handleTabMoved (int from, int to) {

    Q_UNUSED(from);
    Q_UNUSED(to);

    writeSettings();
}

void SeerVariableManagerWidget::handleTabChanged (int index) {

    Q_UNUSED(index);

    writeSettings();
}

void SeerVariableManagerWidget::handleRaiseLoggerTab () {

    int idx = tabWidget->indexOf(_variableLoggerBrowserWidget);

    if (idx < 0) {
        return;
    }

    tabWidget->setCurrentIndex(idx);
}

void SeerVariableManagerWidget::handleRaiseTrackerTab () {

    int idx = tabWidget->indexOf(_variableTrackerBrowserWidget);

    if (idx < 0) {
        return;
    }

    tabWidget->setCurrentIndex(idx);
}

void SeerVariableManagerWidget::writeSettings () {

    // Write tab order to settings.
    QStringList tabs;

    for (int i=0; i<tabWidget->tabBar()->count(); i++) {
        tabs.append(tabWidget->tabBar()->tabText(i));
    }

    QString current = tabWidget->tabBar()->tabText(tabWidget->tabBar()->currentIndex());

    //qDebug() << "Tabs" << tabs;
    //qDebug() << "Current" << current;

    QSettings settings;

    settings.beginGroup("variablemanagerwindow"); {
        settings.setValue("taborder",   tabs.join(','));
        settings.setValue("tabcurrent", current);
    } settings.endGroup();
}

void SeerVariableManagerWidget::readSettings () {

    // Can't move things?
    if (tabWidget->tabBar()->isMovable() == false) {
        return;
    }

    // Read tab order from settings.
    QSettings   settings;
    QStringList tabs;
    QString     current;

    settings.beginGroup("variablemanagerwindow"); {
        tabs    = settings.value("taborder").toString().split(',');
        current = settings.value("tabcurrent").toString();
    } settings.endGroup();

    //qDebug() << "Tabs"    << tabs;
    //qDebug() << "Current" << current;

    // Move tabs to the requested order.
    for (int i=0; i<tabs.count(); i++) {

        QString tab = tabs[i];
        int     tb  = -1;

        for (int j=0; j<tabWidget->tabBar()->count(); j++) {
            if (tabWidget->tabBar()->tabText(j) == tab) {
                tb = j;
                break;
            }
        }

        if (tb != -1) {
            tabWidget->tabBar()->moveTab(tb, i);
        }
    }

    // Make a tab current.
    if (current != "") {
        for (int i=0; i<tabWidget->tabBar()->count(); i++) {
            if (tabWidget->tabBar()->tabText(i) == current) {
                tabWidget->setCurrentIndex(i);
                break;
            }
        }
    }else{
        tabWidget->setCurrentIndex(0);
    }
}

