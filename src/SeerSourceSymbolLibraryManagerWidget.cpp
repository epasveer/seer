#include "SeerSourceSymbolLibraryManagerWidget.h"
#include "SeerHelpPageDialog.h"
#include "QHContainerWidget.h"
#include <QtWidgets/QToolButton>
#include <QtGui/QIcon>
#include <QtCore/QDebug>

SeerSourceSymbolLibraryManagerWidget::SeerSourceSymbolLibraryManagerWidget (QWidget* parent) : QWidget(parent) {

    // Initialize private data

    // Setup UI
    setupUi(this);

    // Setup the widgets
    tabWidget->setMovable(true);
    tabWidget->setTabsClosable(false);

    _sourceBrowserWidget    = new SeerSourceBrowserWidget(this);
    _functionBrowserWidget  = new SeerFunctionBrowserWidget(this);
    _typeBrowserWidget      = new SeerTypeBrowserWidget(this);
    _staticBrowserWidget    = new SeerStaticBrowserWidget(this);
    _libraryBrowserWidget   = new SeerLibraryBrowserWidget(this);

    tabWidget->addTab(_sourceBrowserWidget,    "Source");
    tabWidget->addTab(_functionBrowserWidget,  "Functions");
    tabWidget->addTab(_typeBrowserWidget,      "Types");
    tabWidget->addTab(_staticBrowserWidget,    "Statics");
    tabWidget->addTab(_libraryBrowserWidget,   "Libraries");

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

    // Connect things.
    QObject::connect(refreshToolButton, &QToolButton::clicked,     this,  &SeerSourceSymbolLibraryManagerWidget::handleRefreshToolButtonClicked);
    QObject::connect(helpToolButton,    &QToolButton::clicked,     this,  &SeerSourceSymbolLibraryManagerWidget::handleHelpToolButtonClicked);
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

void SeerSourceSymbolLibraryManagerWidget::handleRefreshToolButtonClicked () {

    sourceBrowserWidget()->refresh();
    functionBrowserWidget()->refresh();
    typeBrowserWidget()->refresh();
    staticBrowserWidget()->refresh();
    libraryBrowserWidget()->refresh();
}

void SeerSourceSymbolLibraryManagerWidget::handleHelpToolButtonClicked () {

    SeerHelpPageDialog* help = new SeerHelpPageDialog;
    help->loadFile(":/seer/resources/help/SourceSymbolLibraryInfoBrowser.md");
    help->show();
    help->raise();
}

