#include "SeerStackManagerWidget.h"
#include "SeerHelpPageDialog.h"
#include "SeerUtl.h"
#include "QHContainerWidget.h"
#include <QtWidgets/QToolButton>
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

    tabWidget->addTab(_stackFramesBrowserWidget,    "Frames");
    tabWidget->addTab(_stackArgumentsBrowserWidget, "Arguments");
    tabWidget->addTab(_stackLocalsBrowserWidget,    "Locals");

    QToolButton* refreshToolButton = new QToolButton(tabWidget);
    refreshToolButton->setIcon(QIcon(":/seer/resources/RelaxLightIcons/view-refresh.svg"));
    refreshToolButton->setToolTip("Refresh the stack information.");

    QToolButton* helpToolButton = new QToolButton(tabWidget);
    helpToolButton->setIcon(QIcon(":/seer/resources/RelaxLightIcons/help-about.svg"));
    helpToolButton->setToolTip("Help on stack information.");

    QHContainerWidget* hcontainer = new QHContainerWidget(this);
    hcontainer->setSpacing(3);
    hcontainer->addWidget(refreshToolButton);
    hcontainer->addWidget(helpToolButton);

    tabWidget->setCornerWidget(hcontainer, Qt::TopRightCorner);

    // Restore tab ordering.
    readSettings();

    // Connect things.
    QObject::connect(refreshToolButton,    &QToolButton::clicked,         this,  &SeerStackManagerWidget::handleRefreshToolButtonClicked);
    QObject::connect(helpToolButton,       &QToolButton::clicked,         this,  &SeerStackManagerWidget::handleHelpToolButtonClicked);
    QObject::connect(tabWidget->tabBar(),  &QTabBar::tabMoved,            this,  &SeerStackManagerWidget::handleTabMoved);
    QObject::connect(tabWidget->tabBar(),  &QTabBar::currentChanged,      this,  &SeerStackManagerWidget::handleTabChanged);
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

void SeerStackManagerWidget::handleRefreshToolButtonClicked () {

    stackFramesBrowserWidget()->refresh();
    stackArgumentsBrowserWidget()->refresh();
    stackLocalsBrowserWidget()->refresh();

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

    // Write tab order to settings.
    QStringList tabs;

    for (int i=0; i<tabWidget->tabBar()->count(); i++) {
        tabs.append(tabWidget->tabBar()->tabText(i));
    }

    QString current = tabWidget->tabBar()->tabText(tabWidget->tabBar()->currentIndex());

    //qDebug() << "Tabs"    << tabs;
    //qDebug() << "Current" << current;

    QSettings settings;

    settings.beginGroup("stackmanagerwindow"); {
        settings.setValue("taborder",   tabs.join(','));
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
    QString     current;

    settings.beginGroup("stackmanagerwindow"); {
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

        label->setText("Stack Info for Thread Id : " + currentthreadid_text);

        stackFramesBrowserWidget()->refresh();
        stackArgumentsBrowserWidget()->refresh();
        stackLocalsBrowserWidget()->refresh();

    }else if (text.startsWith("^error,msg=\"No registers.\"")) {

        label->setText("Stack Info");

    }else{
        // Ignore others.
    }

    QApplication::restoreOverrideCursor();
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


