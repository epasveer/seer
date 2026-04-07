// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "SeerVariableManagerWidget.h"
#include "SeerHelpPageDialog.h"
#include "QHContainerWidget.h"
#include <QtWidgets/QToolButton>
#include <QtWidgets/QMenu>
#include <QtWidgets/QCheckBox>
#include <QtWidgets/QWidgetAction>
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
    _signalValuesBrowserWidget    = new SeerSignalValuesBrowserWidget(this);

    tabWidget->addTab(_variableLoggerBrowserWidget,  "Logger");
    tabWidget->addTab(_variableTrackerBrowserWidget, "Tracker");
    tabWidget->addTab(_registerValuesBrowserWidget,  "Registers");
    tabWidget->addTab(_signalValuesBrowserWidget,    "Signals");

    QToolButton* tabsContextMenuButton = new QToolButton(tabWidget);
    tabsContextMenuButton->setIcon(QIcon(":/seer/resources/thenounproject/preferences.svg"));
    tabsContextMenuButton->setToolTip("Show/Hide tabs.");
    tabsContextMenuButton->setContextMenuPolicy(Qt::CustomContextMenu);

    QToolButton* refreshToolButton = new QToolButton(tabWidget);
    refreshToolButton->setIcon(QIcon(":/seer/resources/RelaxLightIcons/view-refresh.svg"));
    refreshToolButton->setToolTip("Refresh the variable/register/signal information.");

    QToolButton* helpToolButton = new QToolButton(tabWidget);
    helpToolButton->setIcon(QIcon(":/seer/resources/RelaxLightIcons/help-about.svg"));
    helpToolButton->setToolTip("Help on variable/register/signal information.");

    QHContainerWidget* hcontainer = new QHContainerWidget(this);
    hcontainer->setSpacing(3);
    hcontainer->addWidget(tabsContextMenuButton);
    hcontainer->addWidget(refreshToolButton);
    hcontainer->addWidget(helpToolButton);

    tabWidget->setCornerWidget(hcontainer, Qt::TopRightCorner);

    // Restore tab ordering.
    readSettings();

    // Connect things.
    QObject::connect(tabsContextMenuButton,         &QToolButton::clicked,                          this,  &SeerVariableManagerWidget::handleTabsContextMenuButtonClicked);
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

SeerSignalValuesBrowserWidget* SeerVariableManagerWidget::signalValuesBrowserWidget () {
    return _signalValuesBrowserWidget;
}

void SeerVariableManagerWidget::handleRefreshToolButtonClicked () {

    variableTrackerBrowserWidget()->refresh();
    registerValuesBrowserWidget()->refresh();
    signalValuesBrowserWidget()->refresh();
}

void SeerVariableManagerWidget::handleHelpToolButtonClicked () {

    SeerHelpPageDialog* help = new SeerHelpPageDialog;
    help->loadFile(":/seer/resources/help/VariableRegisterSignalInfoBrowser.md");
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

    // Build up visible list.
    QStringList visible;

    for (int i=0; i<tabWidget->tabBar()->count(); i++) {
        visible.append(tabWidget->isTabVisible(i) ? "true" : "false");
    }

    // Build up tab order.
    QStringList tabs;

    for (int i=0; i<tabWidget->tabBar()->count(); i++) {
        tabs.append(tabWidget->tabBar()->tabText(i));
    }

    // Build up current tab.
    QString current = tabWidget->tabBar()->tabText(tabWidget->tabBar()->currentIndex());

    //qDebug() << "Tabs" << tabs;
    //qDebug() << "Current" << current;

    QSettings settings;

    settings.beginGroup("variablemanagerwindow"); {
        settings.setValue("taborder",   tabs.join(','));
        settings.setValue("tabvisible", visible.join(','));
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
    QStringList visible;
    QString     current;

    settings.beginGroup("variablemanagerwindow"); {
        tabs    = settings.value("taborder").toString().split(',');
        visible = settings.value("tabvisible").toString().split(',');
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

    // Show/Hide tabs.
    for (int i=0; i<visible.count(); i++) {
        QString flag = visible[i];
        if (flag == "true") {
            tabWidget->setTabVisible(i,true);
        }else if (flag == "false") {
            tabWidget->setTabVisible(i,false);
        }else{
            tabWidget->setTabVisible(i,true);
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

void SeerVariableManagerWidget::handleTabsContextMenuButtonClicked() {

    // Build the menu and execute it.
    QMenu        contextMenu;
    QWidget*     container = new QWidget(&contextMenu);
    QVBoxLayout* layout    = new QVBoxLayout(container);

    for (int i = 0; i < tabWidget->count(); i++) {

        QString title = tabWidget->tabText(i);
        QCheckBox* showTabCheckBox = new QCheckBox(title, container);
        showTabCheckBox->setChecked(tabWidget->isTabVisible(i));
        layout->addWidget(showTabCheckBox);

        QObject::connect(showTabCheckBox, &QCheckBox::toggled, [this, showTabCheckBox, i](bool checked) {

            // Count visible tabs.
            int count=0;

            for (int x=0; x<tabWidget->count(); x++) {
                if (tabWidget->isTabVisible(x)) {
                    count++;
                }
            }

            // Adjust the count. The 'checked' is made before the UI is updated.
            if (checked == true) {
                count++;
            }else{
                count--;
            }

            // Reset the checkbox UI if the last visible tab was clicked.
            if (checked == false and count == 0) {
                showTabCheckBox->setChecked(true);
            // Don't hide last visible tab.
            }else if (checked == false and count > 1) {
                tabWidget->setTabVisible(i, checked);
            // Always show tabs when asked.
            }else{
                tabWidget->setTabVisible(i, checked);
            }

            writeSettings();
        });
    }
    container->setLayout(layout);

    QWidgetAction* action = new QWidgetAction(&contextMenu);
    action->setDefaultWidget(container);
    contextMenu.addAction(action);

    contextMenu.exec(QCursor::pos());
}

