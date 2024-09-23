#include "SeerCudaVisualizerWidget.h"
#include "SeerHelpPageDialog.h"
#include "QHContainerWidget.h"
#include <QtWidgets/QToolButton>
#include <QtWidgets/QTextBrowser>
#include <QtWidgets/QTabBar>
#include <QtGui/QIcon>
#include <QtCore/QFile>
#include <QtCore/QSettings>
#include <QtCore/QDebug>

SeerCudaVisualizerWidget::SeerCudaVisualizerWidget (QWidget* parent) : QWidget(parent) {

    // Initialize private data

    // Setup UI
    setupUi(this);

    // Setup the widgets
    tabWidget->setMovable(true);
    tabWidget->setTabsClosable(false);

    _cudaDevicesBrowserWidget = new SeerCudaDevicesBrowserWidget(this);

    tabWidget->addTab(_cudaDevicesBrowserWidget, "Devices");

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
    QObject::connect(refreshToolButton,           &QToolButton::clicked,                                 this,  &SeerCudaVisualizerWidget::handleRefreshToolButtonClicked);
    QObject::connect(helpToolButton,              &QToolButton::clicked,                                 this,  &SeerCudaVisualizerWidget::handleHelpToolButtonClicked);
    QObject::connect(tabWidget->tabBar(),         &QTabBar::tabMoved,                                    this,  &SeerCudaVisualizerWidget::handleTabMoved);
    QObject::connect(tabWidget->tabBar(),         &QTabBar::currentChanged,                              this,  &SeerCudaVisualizerWidget::handleTabChanged);
    QObject::connect(cudaDevicesBrowserWidget(),  &SeerCudaDevicesBrowserWidget::refreshCudaDevices,     this,  &SeerCudaVisualizerWidget::handleRefreshCudaDevices);
}

SeerCudaVisualizerWidget::~SeerCudaVisualizerWidget () {
}

SeerCudaDevicesBrowserWidget* SeerCudaVisualizerWidget::cudaDevicesBrowserWidget () {
    return _cudaDevicesBrowserWidget;
}

void SeerCudaVisualizerWidget::handleRefreshToolButtonClicked () {

    qDebug() << "Calling 'refresh' on each CUDA widget";

    cudaDevicesBrowserWidget()->refresh();
}

void SeerCudaVisualizerWidget::handleHelpToolButtonClicked () {

    SeerHelpPageDialog* help = new SeerHelpPageDialog;
    help->loadFile(":/seer/resources/help/CudaInfoBrowser.md");
    help->show();
    help->raise();
}

void SeerCudaVisualizerWidget::handleTabMoved (int from, int to) {

    Q_UNUSED(from);
    Q_UNUSED(to);

    writeSettings();
}

void SeerCudaVisualizerWidget::handleTabChanged (int index) {

    Q_UNUSED(index);

    writeSettings();
}

void SeerCudaVisualizerWidget::handleRefreshCudaDevices () {

    qDebug() << "Forward 'refreshCudaDevices' signal";

    emit refreshCudaDevices();
}

void SeerCudaVisualizerWidget::handleText (const QString& text) {

    qDebug() << "Calling 'handleText' on each CUDA widget";

    cudaDevicesBrowserWidget()->handleText(text);
}

void SeerCudaVisualizerWidget::handleStoppingPointReached () {

    qDebug() << "Calling 'handleStoppingPointReached' on each CUDA widget";

    cudaDevicesBrowserWidget()->handleStoppingPointReached();
}

void SeerCudaVisualizerWidget::writeSettings () {

    // Write tab order to settings.
    QStringList tabs;

    for (int i=0; i<tabWidget->tabBar()->count(); i++) {
        tabs.append(tabWidget->tabBar()->tabText(i));
    }

    QString current = tabWidget->tabBar()->tabText(tabWidget->tabBar()->currentIndex());

    //qDebug() << "Tabs"    << tabs;
    //qDebug() << "Current" << current;

    QSettings settings;

    settings.beginGroup("cudavisualizerwindow"); {
        settings.setValue("taborder", tabs.join(','));
        settings.setValue("tabcurrent", current);
        settings.setValue("size", size());
    } settings.endGroup();
}

void SeerCudaVisualizerWidget::readSettings () {

    // Can't move things?
    if (tabWidget->tabBar()->isMovable() == false) {
        return;
    }

    // Read tab order from settings.
    QSettings   settings;
    QStringList tabs;
    QString     current;

    settings.beginGroup("cudavisualizerwindow"); {
        tabs    = settings.value("taborder").toString().split(',');
        current = settings.value("tabcurrent").toString();
        resize(settings.value("size", QSize(800, 400)).toSize());
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

void SeerCudaVisualizerWidget::resizeEvent (QResizeEvent* event) {

    writeSettings();

    QWidget::resizeEvent(event);
}


