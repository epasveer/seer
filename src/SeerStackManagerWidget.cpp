// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "SeerStackManagerWidget.h"
#include "SeerHelpPageDialog.h"
#include "SeerUtl.h"
#include "QHContainerWidget.h"
#include <QtWidgets/QToolButton>
#include <QtWidgets/QMenu>
#include <QtWidgets/QCheckBox>
#include <QtWidgets/QWidgetAction>
#include <QtGui/QIcon>
#include <QtCore/QSettings>
#include <QtCore/QDebug>

SeerStackManagerWidget::SeerStackManagerWidget (QWidget* parent) : QWidget(parent) {

    // Initialize private data

    // Setup UI
    setupUi(this);

    // Setup the widgets
    tabWidget->setMovable(true);
    tabWidget->setTabsClosable(false);

    _stackFramesBrowserWidget    = new SeerStackFramesBrowserWidget(this);
    _stackArgumentsBrowserWidget = new SeerStackArgumentsBrowserWidget(this);
    _stackLocalsBrowserWidget    = new SeerStackLocalsBrowserWidget(this);
    _stackDumpBrowserWidget      = new SeerStackDumpBrowserWidget(this);

    tabWidget->addTab(_stackFramesBrowserWidget,    "Frames");
    tabWidget->addTab(_stackArgumentsBrowserWidget, "Arguments");
    tabWidget->addTab(_stackLocalsBrowserWidget,    "Locals");
    tabWidget->addTab(_stackDumpBrowserWidget,      "Stack");

    QToolButton* tabsContextMenuButton = new QToolButton(tabWidget);
    tabsContextMenuButton->setIcon(QIcon(":/seer/resources/thenounproject/preferences.svg"));
    tabsContextMenuButton->setToolTip("Show/Hide tabs.");
    tabsContextMenuButton->setContextMenuPolicy(Qt::CustomContextMenu);

    QToolButton* refreshToolButton = new QToolButton(tabWidget);
    refreshToolButton->setIcon(QIcon(":/seer/resources/RelaxLightIcons/view-refresh.svg"));
    refreshToolButton->setToolTip("Refresh the stack information.");

    QToolButton* helpToolButton = new QToolButton(tabWidget);
    helpToolButton->setIcon(QIcon(":/seer/resources/RelaxLightIcons/help-about.svg"));
    helpToolButton->setToolTip("Help on stack information.");

    QHContainerWidget* hcontainer = new QHContainerWidget(this);
    hcontainer->setSpacing(3);
    hcontainer->addWidget(tabsContextMenuButton);
    hcontainer->addWidget(refreshToolButton);
    hcontainer->addWidget(helpToolButton);

    tabWidget->setCornerWidget(hcontainer, Qt::TopRightCorner);

    // Restore tab ordering.
    readSettings();

    // Connect things.
    QObject::connect(tabsContextMenuButton, &QToolButton::clicked,         this,  &SeerStackManagerWidget::handleTabsContextMenuButtonClicked);
    QObject::connect(refreshToolButton,     &QToolButton::clicked,         this,  &SeerStackManagerWidget::handleRefreshToolButtonClicked);
    QObject::connect(helpToolButton,        &QToolButton::clicked,         this,  &SeerStackManagerWidget::handleHelpToolButtonClicked);
    QObject::connect(tabWidget->tabBar(),   &QTabBar::tabMoved,            this,  &SeerStackManagerWidget::handleTabMoved);
    QObject::connect(tabWidget->tabBar(),   &QTabBar::currentChanged,      this,  &SeerStackManagerWidget::handleTabChanged);
}

SeerStackManagerWidget::~SeerStackManagerWidget () {
}

SeerStackFramesBrowserWidget* SeerStackManagerWidget::stackFramesBrowserWidget () {
    return _stackFramesBrowserWidget;
}

SeerStackArgumentsBrowserWidget* SeerStackManagerWidget::stackArgumentsBrowserWidget () {
    return _stackArgumentsBrowserWidget;
}

SeerStackLocalsBrowserWidget* SeerStackManagerWidget::stackLocalsBrowserWidget () {
    return _stackLocalsBrowserWidget;
}

SeerStackDumpBrowserWidget* SeerStackManagerWidget::stackDumpBrowserWidget () {
    return _stackDumpBrowserWidget;
}

void SeerStackManagerWidget::handleRefreshToolButtonClicked () {

    stackFramesBrowserWidget()->refresh();
    stackArgumentsBrowserWidget()->refresh();
    stackLocalsBrowserWidget()->refresh();
    stackDumpBrowserWidget()->refresh();

    refresh();
}

void SeerStackManagerWidget::handleHelpToolButtonClicked () {

    SeerHelpPageDialog* help = new SeerHelpPageDialog;
    help->loadFile(":/seer/resources/help/StackInfoBrowser.md");
    help->show();
    help->raise();
}

void SeerStackManagerWidget::handleTabMoved (int from, int to) {

    Q_UNUSED(from);
    Q_UNUSED(to);

    writeSettings();
}

void SeerStackManagerWidget::handleTabChanged (int index) {

    Q_UNUSED(index);

    writeSettings();
}

void SeerStackManagerWidget::writeSettings () {

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

    QSettings settings;

    settings.beginGroup("stackmanagerwindow"); {
        settings.setValue("taborder",   tabs.join(','));
        settings.setValue("tabvisible", visible.join(','));
        settings.setValue("tabcurrent", current);
    } settings.endGroup();
}

void SeerStackManagerWidget::readSettings () {

    // Can't move things?
    if (tabWidget->tabBar()->isMovable() == false) {
        return;
    }

    // Read tab order from settings.
    QSettings   settings;
    QStringList tabs;
    QStringList visible;
    QString     current;

    settings.beginGroup("stackmanagerwindow"); {
        tabs    = settings.value("taborder").toString().split(',');
        visible = settings.value("tabvisible").toString().split(',');
        current = settings.value("tabcurrent").toString();
    } settings.endGroup();

    //qDebug() << "Tabs"    << tabs;
    //qDebug() << "Current" << current;

    // Move tabs to the requested order.
    // Ignore signals from the tabbar as these will call 'writeSettings'
    // while we're in 'readSettings'.
    {
        QSignalBlocker blocker(tabWidget->tabBar());

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

void SeerStackManagerWidget::handleText (const QString& text) {

    // Don't do any work if the widget is hidden.
    if (isHidden()) {
        return;
    }

    QApplication::setOverrideCursor(Qt::BusyCursor);

    if (text.startsWith("^done,thread-ids={")) {

        QString newtext = Seer::filterEscapes(text); // Filter escaped characters.

        // ^done,thread-ids={
        //        thread-id=\"1\",
        //        thread-id=\"2\"
        //    },
        //    current-thread-id=\"1\",
        //    number-of-threads=\"2\"

        QString currentthreadid_text = Seer::parseFirst(newtext,   "current-thread-id=", '"', '"', false);

        if (currentthreadid_text == "") {
            label->setText("Stack Info - No Thread Selected");
        }else{
            label->setText("Stack Info - Thread Id " + currentthreadid_text);
        }

        stackFramesBrowserWidget()->refresh();
        stackArgumentsBrowserWidget()->refresh();
        stackLocalsBrowserWidget()->refresh();
        stackDumpBrowserWidget()->refresh();

    }else{
        // Ignore others.
    }

    QApplication::restoreOverrideCursor();
}

void SeerStackManagerWidget::handleSessionTerminated () {

    label->setText("Stack Info");
}

void SeerStackManagerWidget::handleStoppingPointReached () {

    // Don't do any work if the widget is hidden.
    if (isHidden()) {
        return;
    }

    refresh();
}

void SeerStackManagerWidget::refresh () {

    emit refreshThreadFrames();
}

void SeerStackManagerWidget::handleTabsContextMenuButtonClicked() {

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


