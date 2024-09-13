#include "SeerCudaManagerWidget.h"
#include "SeerHelpPageDialog.h"
#include "QHContainerWidget.h"
#include <QtWidgets/QToolButton>
#include <QtWidgets/QTextBrowser>
#include <QtWidgets/QTabBar>
#include <QtGui/QIcon>
#include <QtCore/QFile>
#include <QtCore/QSettings>
#include <QtCore/QDebug>

SeerCudaManagerWidget::SeerCudaManagerWidget (QWidget* parent) : QWidget(parent) {

    // Initialize private data

    // Setup UI
    setupUi(this);

    // Setup the widgets
    tabWidget->setMovable(true);
    tabWidget->setTabsClosable(false);

    _cudaDevicesBrowserWidget = new SeerCudaDevicesBrowserWidget(this);

    tabWidget->addTab(_cudaDevicesBrowserWidget,     "Devices");

    QToolButton* refreshToolButton = new QToolButton(tabWidget);
    refreshToolButton->setIcon(QIcon(":/seer/resources/RelaxLightIcons/view-refresh.svg"));
    refreshToolButton->setToolTip("Refresh the cuda information.");

    QToolButton* helpToolButton = new QToolButton(tabWidget);
    helpToolButton->setIcon(QIcon(":/seer/resources/RelaxLightIcons/help-about.svg"));
    helpToolButton->setToolTip("Help on cuda information.");

    QHContainerWidget* hcontainer = new QHContainerWidget(this);
    hcontainer->setSpacing(3);
    hcontainer->addWidget(refreshToolButton);
    hcontainer->addWidget(helpToolButton);

    tabWidget->setCornerWidget(hcontainer, Qt::TopRightCorner);

    // Restore tab ordering.
    readSettings();

    // Connect things.
    QObject::connect(refreshToolButton,    &QToolButton::clicked,              this,  &SeerCudaManagerWidget::handleRefreshToolButtonClicked);
    QObject::connect(helpToolButton,       &QToolButton::clicked,              this,  &SeerCudaManagerWidget::handleHelpToolButtonClicked);
    QObject::connect(tabWidget->tabBar(),  &QTabBar::tabMoved,                 this,  &SeerCudaManagerWidget::handleTabMoved);
    QObject::connect(tabWidget->tabBar(),  &QTabBar::currentChanged,           this,  &SeerCudaManagerWidget::handleTabChanged);
}

SeerCudaManagerWidget::~SeerCudaManagerWidget () {
}

SeerCudaDevicesBrowserWidget* SeerCudaManagerWidget::cudaDevicesBrowserWidget () {
    return _cudaDevicesBrowserWidget;
}

void SeerCudaManagerWidget::handleRefreshToolButtonClicked () {

    cudaDevicesBrowserWidget()->refresh();
}

void SeerCudaManagerWidget::handleHelpToolButtonClicked () {

    SeerHelpPageDialog* help = new SeerHelpPageDialog;
    help->loadFile(":/seer/resources/help/CudaInfoBrowser.md");
    help->show();
    help->raise();
}

void SeerCudaManagerWidget::handleTabMoved (int from, int to) {

    Q_UNUSED(from);
    Q_UNUSED(to);

    writeSettings();
}

void SeerCudaManagerWidget::handleTabChanged (int index) {

    Q_UNUSED(index);

    writeSettings();
}

void SeerCudaManagerWidget::writeSettings () {

    // Write tab order to settings.
    QStringList tabs;

    for (int i=0; i<tabWidget->tabBar()->count(); i++) {
        tabs.append(tabWidget->tabBar()->tabText(i));
    }

    QString current = tabWidget->tabBar()->tabText(tabWidget->tabBar()->currentIndex());

    //qDebug() << "Tabs"    << tabs;
    //qDebug() << "Current" << current;

    QSettings settings;

    settings.beginGroup("cudamanagerwindow"); {
        settings.setValue("taborder", tabs.join(','));
        settings.setValue("tabcurrent", current);
    } settings.endGroup();
}

void SeerCudaManagerWidget::readSettings () {

    // Can't move things?
    if (tabWidget->tabBar()->isMovable() == false) {
        return;
    }

    // Read tab order from settings.
    QSettings   settings;
    QStringList tabs;
    QString     current;

    settings.beginGroup("cudamanagerwindow"); {
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

