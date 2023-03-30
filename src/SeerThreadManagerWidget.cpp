#include "SeerThreadManagerWidget.h"
#include "SeerHelpPageDialog.h"
#include "QHContainerWidget.h"
#include <QtWidgets/QToolButton>
#include <QtWidgets/QTextBrowser>
#include <QtGui/QIcon>
#include <QtCore/QFile>
#include <QtCore/QSettings>
#include <QtCore/QDebug>

SeerThreadManagerWidget::SeerThreadManagerWidget (QWidget* parent) : QWidget(parent) {

    // Initialize private data

    // Setup UI
    setupUi(this);

    // Setup the widgets
    tabWidget->setMovable(true);
    tabWidget->setTabsClosable(false);

    _threadFramesBrowserWidget = new SeerThreadFramesBrowserWidget(this);
    _threadIdsBrowserWidget    = new SeerThreadIdsBrowserWidget(this);
    _threadGroupsBrowserWidget = new SeerThreadGroupsBrowserWidget(this);
    _adaTasksBrowserWidget     = new SeerAdaTasksBrowserWidget(this);

    tabWidget->addTab(_threadFramesBrowserWidget, "Frames");
    tabWidget->addTab(_threadIdsBrowserWidget,    "Ids");
    tabWidget->addTab(_threadGroupsBrowserWidget, "Groups");
    tabWidget->addTab(_adaTasksBrowserWidget,     "AdaTasks");

    QToolButton* refreshToolButton = new QToolButton(tabWidget);
    refreshToolButton->setIcon(QIcon(":/seer/resources/RelaxLightIcons/view-refresh.svg"));
    refreshToolButton->setToolTip("Refresh the thread information.");

    QToolButton* helpToolButton = new QToolButton(tabWidget);
    helpToolButton->setIcon(QIcon(":/seer/resources/RelaxLightIcons/help-about.svg"));
    helpToolButton->setToolTip("Help on thread information.");

    QHContainerWidget* hcontainer = new QHContainerWidget(this);
    hcontainer->setSpacing(3);
    hcontainer->addWidget(refreshToolButton);
    hcontainer->addWidget(helpToolButton);

    tabWidget->setCornerWidget(hcontainer, Qt::TopRightCorner);

    // Restore tab ordering.
    readSettings();

    // Connect things.
    QObject::connect(refreshToolButton,        &QToolButton::clicked,                                   this,  &SeerThreadManagerWidget::handleRefreshToolButtonClicked);
    QObject::connect(helpToolButton,           &QToolButton::clicked,                                   this,  &SeerThreadManagerWidget::handleHelpToolButtonClicked);
    QObject::connect(schedulerLockingComboBox, QOverload<int>::of(&QComboBox::currentIndexChanged),     this,  &SeerThreadManagerWidget::handleSchedulerLockingComboBox);
    QObject::connect(scheduleMultipleComboBox, QOverload<int>::of(&QComboBox::currentIndexChanged),     this,  &SeerThreadManagerWidget::handleScheduleMultipleComboBox);
    QObject::connect(forkFollowsComboBox,      QOverload<int>::of(&QComboBox::currentIndexChanged),     this,  &SeerThreadManagerWidget::handleForkFollowComboBox);
    QObject::connect(tabWidget->tabBar(),      &QTabBar::tabMoved,                                      this,  &SeerThreadManagerWidget::handleTabMoved);
    QObject::connect(tabWidget->tabBar(),      &QTabBar::currentChanged,                                this,  &SeerThreadManagerWidget::handleTabChanged);
}

SeerThreadManagerWidget::~SeerThreadManagerWidget () {
}

SeerThreadFramesBrowserWidget* SeerThreadManagerWidget::threadFramesBrowserWidget () {
    return _threadFramesBrowserWidget;
}

SeerThreadIdsBrowserWidget* SeerThreadManagerWidget::threadIdsBrowserWidget () {
    return _threadIdsBrowserWidget;
}

SeerThreadGroupsBrowserWidget* SeerThreadManagerWidget::threadGroupsBrowserWidget () {
    return _threadGroupsBrowserWidget;
}

SeerAdaTasksBrowserWidget* SeerThreadManagerWidget::adaTasksBrowserWidget () {
    return _adaTasksBrowserWidget;
}

void SeerThreadManagerWidget::setSchedulerLockingMode (const QString& mode) {

    schedulerLockingComboBox->setCurrentText(mode);
}

QString SeerThreadManagerWidget::schedulerLockingMode () const {

    return schedulerLockingComboBox->currentText();
}

void SeerThreadManagerWidget::setScheduleMultipleMode (const QString& mode) {

    scheduleMultipleComboBox->setCurrentText(mode);
}

QString SeerThreadManagerWidget::scheduleMultipleMode () const {

    return scheduleMultipleComboBox->currentText();
}

void SeerThreadManagerWidget::setForkFollowsMode (const QString& mode) {

    forkFollowsComboBox->setCurrentText(mode);
}

QString SeerThreadManagerWidget::forkFollowsMode () const {

    return forkFollowsComboBox->currentText();
}

void SeerThreadManagerWidget::handleRefreshToolButtonClicked () {

    threadFramesBrowserWidget()->refresh();
    threadIdsBrowserWidget()->refresh();
    threadGroupsBrowserWidget()->refresh();
    adaTasksBrowserWidget()->refresh();
}

void SeerThreadManagerWidget::handleHelpToolButtonClicked () {

    SeerHelpPageDialog* help = new SeerHelpPageDialog;
    help->loadFile(":/seer/resources/help/ThreadProcessInfoBrowser.md");
    help->show();
    help->raise();
}

void SeerThreadManagerWidget::handleSchedulerLockingComboBox (int index) {

    Q_UNUSED(index);

    emit schedulerLockingModeChanged(schedulerLockingComboBox->currentText());
}

void SeerThreadManagerWidget::handleScheduleMultipleComboBox (int index) {

    Q_UNUSED(index);

    emit scheduleMultipleModeChanged(scheduleMultipleComboBox->currentText());
}

void SeerThreadManagerWidget::handleForkFollowComboBox (int index) {

    Q_UNUSED(index);

    emit forkFollowsModeChanged(forkFollowsComboBox->currentText());
}

void SeerThreadManagerWidget::handleTabMoved (int from, int to) {

    Q_UNUSED(from);
    Q_UNUSED(to);

    writeSettings();
}

void SeerThreadManagerWidget::handleTabChanged (int index) {

    Q_UNUSED(index);

    writeSettings();
}

void SeerThreadManagerWidget::writeSettings () {

    // Write tab order to settings.
    QStringList tabs;

    for (int i=0; i<tabWidget->tabBar()->count(); i++) {
        tabs.append(tabWidget->tabBar()->tabText(i));
    }

    QString current = tabWidget->tabBar()->tabText(tabWidget->tabBar()->currentIndex());

    //qDebug() << "Tabs"    << tabs;
    //qDebug() << "Current" << current;

    QSettings settings;

    settings.beginGroup("threadmanagerwindow"); {
        settings.setValue("taborder", tabs.join(','));
        settings.setValue("tabcurrent", current);
    } settings.endGroup();
}

void SeerThreadManagerWidget::readSettings () {

    // Can't move things?
    if (tabWidget->tabBar()->isMovable() == false) {
        return;
    }

    // Read tab order from settings.
    QSettings   settings;
    QStringList tabs;
    QString     current;

    settings.beginGroup("threadmanagerwindow"); {
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

