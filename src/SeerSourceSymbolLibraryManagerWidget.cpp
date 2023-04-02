#include "SeerSourceSymbolLibraryManagerWidget.h"
#include "SeerHelpPageDialog.h"
#include "QHContainerWidget.h"
#include <QtWidgets/QToolButton>
#include <QtGui/QIcon>
#include <QtCore/QSettings>
#include <QtCore/QDebug>

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

    tabWidget->addTab(_sourceBrowserWidget,         "Source");
    tabWidget->addTab(_functionBrowserWidget,       "Functions");
    tabWidget->addTab(_typeBrowserWidget,           "Types");
    tabWidget->addTab(_staticBrowserWidget,         "Statics");
    tabWidget->addTab(_libraryBrowserWidget,        "Libraries");
    tabWidget->addTab(_adaExceptionsBrowserWidget,  "AdaExceptions");

    QToolButton* refreshToolButton = new QToolButton(tabWidget);
    refreshToolButton->setIcon(QIcon(":/seer/resources/RelaxLightIcons/view-refresh.svg"));
    refreshToolButton->setToolTip("Refresh the source/symbol/library information.");

    QToolButton* helpToolButton = new QToolButton(tabWidget);
    helpToolButton->setIcon(QIcon(":/seer/resources/RelaxLightIcons/help-about.svg"));
    helpToolButton->setToolTip("Help on source/symbol/library information.");

    QHContainerWidget* hcontainer = new QHContainerWidget(this);
    hcontainer->setSpacing(3);
    hcontainer->addWidget(refreshToolButton);
    hcontainer->addWidget(helpToolButton);

    tabWidget->setCornerWidget(hcontainer, Qt::TopRightCorner);

    // Restore tab ordering.
    readSettings();

    // Connect things.
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

void SeerSourceSymbolLibraryManagerWidget::handleRefreshToolButtonClicked () {

    sourceBrowserWidget()->refresh();
    functionBrowserWidget()->refresh();
    typeBrowserWidget()->refresh();
    staticBrowserWidget()->refresh();
    libraryBrowserWidget()->refresh();
    adaExceptionsBrowserWidget()->refresh();
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

    // Write tab order to settings.
    QStringList tabs;

    for (int i=0; i<tabWidget->tabBar()->count(); i++) {
        tabs.append(tabWidget->tabBar()->tabText(i));
    }

    QString current = tabWidget->tabBar()->tabText(tabWidget->tabBar()->currentIndex());

    //qDebug() << "Tabs"    << tabs;
    //qDebug() << "Current" << current;

    QSettings settings;

    settings.beginGroup("sourcemanagerwindow"); {
        settings.setValue("taborder",   tabs.join(','));
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
    QString     current;

    settings.beginGroup("sourcemanagerwindow"); {
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

