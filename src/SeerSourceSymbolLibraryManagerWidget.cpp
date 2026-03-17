// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "SeerSourceSymbolLibraryManagerWidget.h"
#include "SeerHelpPageDialog.h"
#include "QHContainerWidget.h"
#include <QtWidgets/QToolButton>
#include <QtGui/QIcon>
#include <QtCore/QSettings>
#include <QtCore/QDebug>
#include <QMenu>
#include <QCheckBox>
#include <QWidgetAction>

SeerSourceSymbolLibraryManagerWidget::SeerSourceSymbolLibraryManagerWidget (QWidget* parent) : QWidget(parent) {

    // Initialize private data

    // Setup UI
    setupUi(this);

    // Setup the widgets
    tabWidget->setMovable(true);
    tabWidget->setTabsClosable(false);

    _sourceBrowserWidget        = new SeerSourceBrowserWidget(this);
    _functionBrowserWidget      = new SeerFunctionBrowserWidget(this);
    _typeBrowserWidget          = new SeerTypeBrowserWidget(this);
    _staticBrowserWidget        = new SeerStaticBrowserWidget(this);
    _libraryBrowserWidget       = new SeerLibraryBrowserWidget(this);
    _adaExceptionsBrowserWidget = new SeerAdaExceptionsBrowserWidget(this);
    _skipBrowserWidget          = new SeerSkipBrowserWidget(this);

    tabWidget->addTab(_sourceBrowserWidget,         "Source");
    tabWidget->addTab(_functionBrowserWidget,       "Functions");
    tabWidget->addTab(_typeBrowserWidget,           "Types");
    tabWidget->addTab(_staticBrowserWidget,         "Statics");
    tabWidget->addTab(_libraryBrowserWidget,        "Libraries");
    tabWidget->addTab(_adaExceptionsBrowserWidget,  "AdaExceptions");
    tabWidget->addTab(_skipBrowserWidget,           "Skips");

    QToolButton* contextMenuButton = new QToolButton(tabWidget);
    contextMenuButton->setIcon(QIcon(":/seer/resources/thenounproject/preferences.svg"));
    contextMenuButton->setToolTip("Show/Hide tabs.");
    contextMenuButton->setContextMenuPolicy(Qt::CustomContextMenu);

    QToolButton* refreshToolButton = new QToolButton(tabWidget);
    refreshToolButton->setIcon(QIcon(":/seer/resources/RelaxLightIcons/view-refresh.svg"));
    refreshToolButton->setToolTip("Refresh the source/symbol/library information.");

    QToolButton* helpToolButton = new QToolButton(tabWidget);
    helpToolButton->setIcon(QIcon(":/seer/resources/RelaxLightIcons/help-about.svg"));
    helpToolButton->setToolTip("Help on source/symbol/library information.");

    QHContainerWidget* hcontainer = new QHContainerWidget(this);
    hcontainer->setSpacing(3);
    hcontainer->addWidget(contextMenuButton);
    hcontainer->addWidget(refreshToolButton);
    hcontainer->addWidget(helpToolButton);

    tabWidget->setCornerWidget(hcontainer, Qt::TopRightCorner);

    // Restore tab ordering.
    readSettings();

    // Connect things.
    QObject::connect(contextMenuButton,     &QToolButton::clicked,     this,  &SeerSourceSymbolLibraryManagerWidget::handleShowTabsContextMenuButtonClicked);
    QObject::connect(refreshToolButton,     &QToolButton::clicked,     this,  &SeerSourceSymbolLibraryManagerWidget::handleRefreshToolButtonClicked);
    QObject::connect(helpToolButton,        &QToolButton::clicked,     this,  &SeerSourceSymbolLibraryManagerWidget::handleHelpToolButtonClicked);
    QObject::connect(tabWidget->tabBar(),   &QTabBar::tabMoved,        this,  &SeerSourceSymbolLibraryManagerWidget::handleTabMoved);
    QObject::connect(tabWidget->tabBar(),   &QTabBar::currentChanged,  this,  &SeerSourceSymbolLibraryManagerWidget::handleTabChanged);
}

SeerSourceSymbolLibraryManagerWidget::~SeerSourceSymbolLibraryManagerWidget () {
}

SeerSourceBrowserWidget* SeerSourceSymbolLibraryManagerWidget::sourceBrowserWidget () {
    return _sourceBrowserWidget;
}

SeerFunctionBrowserWidget* SeerSourceSymbolLibraryManagerWidget::functionBrowserWidget () {
    return _functionBrowserWidget;
}

SeerTypeBrowserWidget* SeerSourceSymbolLibraryManagerWidget::typeBrowserWidget () {
    return _typeBrowserWidget;
}

SeerStaticBrowserWidget* SeerSourceSymbolLibraryManagerWidget::staticBrowserWidget () {
    return _staticBrowserWidget;
}

SeerLibraryBrowserWidget* SeerSourceSymbolLibraryManagerWidget::libraryBrowserWidget () {
    return _libraryBrowserWidget;
}

SeerAdaExceptionsBrowserWidget* SeerSourceSymbolLibraryManagerWidget::adaExceptionsBrowserWidget () {
    return _adaExceptionsBrowserWidget;
}

SeerSkipBrowserWidget* SeerSourceSymbolLibraryManagerWidget::skipBrowserWidget () {
    return _skipBrowserWidget;
}

void SeerSourceSymbolLibraryManagerWidget::handleRefreshToolButtonClicked () {

    sourceBrowserWidget()->refresh();
    functionBrowserWidget()->refresh();
    typeBrowserWidget()->refresh();
    staticBrowserWidget()->refresh();
    libraryBrowserWidget()->refresh();
    adaExceptionsBrowserWidget()->refresh();
    skipBrowserWidget()->refresh();
}

void SeerSourceSymbolLibraryManagerWidget::handleHelpToolButtonClicked () {

    SeerHelpPageDialog* help = new SeerHelpPageDialog;
    help->loadFile(":/seer/resources/help/SourceSymbolLibraryInfoBrowser.md");
    help->show();
    help->raise();
}

void SeerSourceSymbolLibraryManagerWidget::handleTabMoved (int from, int to) {

    Q_UNUSED(from);
    Q_UNUSED(to);

    writeSettings();
}

void SeerSourceSymbolLibraryManagerWidget::handleTabChanged (int index) {

    Q_UNUSED(index);

    writeSettings();
}

void SeerSourceSymbolLibraryManagerWidget::writeSettings () {

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

    //qDebug() << "Tabs"    << tabs;
    //qDebug() << "Current" << current;

    // Write settings.
    QSettings settings;

    settings.beginGroup("sourcemanagerwindow"); {
        settings.setValue("taborder",   tabs.join(','));
        settings.setValue("tabvisible", visible.join(','));
        settings.setValue("tabcurrent", current);
    } settings.endGroup();
}

void SeerSourceSymbolLibraryManagerWidget::readSettings () {

    // Can't move things?
    if (tabWidget->tabBar()->isMovable() == false) {
        return;
    }

    // Read tab order from settings.
    QSettings   settings;
    QStringList tabs;
    QStringList visible;
    QString     current;

    settings.beginGroup("sourcemanagerwindow"); {
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

void SeerSourceSymbolLibraryManagerWidget::handleShowTabsContextMenuButtonClicked() {

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

