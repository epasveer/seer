#include "SeerSourceLibraryManagerWidget.h"
#include <QtWidgets/QToolButton>
#include <QtGui/QIcon>
#include <QtCore/QDebug>

SeerSourceLibraryManagerWidget::SeerSourceLibraryManagerWidget (QWidget* parent) : QWidget(parent) {

    // Initialize private data

    // Setup UI
    setupUi(this);

    // Setup the widgets
    tabWidget->setMovable(true);
    tabWidget->setTabsClosable(false);

    _sourceBrowserWidget    = new SeerSourceBrowserWidget(this);
    _functionBrowserWidget  = new SeerFunctionBrowserWidget(this);
    _typeBrowserWidget      = new SeerTypeBrowserWidget(this);
    _variableBrowserWidget  = new SeerVariableBrowserWidget(this);
    _libraryBrowserWidget   = new SeerLibraryBrowserWidget(this);

    tabWidget->addTab(_sourceBrowserWidget,    "Source");
    tabWidget->addTab(_functionBrowserWidget,  "Functions");
    tabWidget->addTab(_typeBrowserWidget,      "Types");
    tabWidget->addTab(_variableBrowserWidget,  "Variables");
    tabWidget->addTab(_libraryBrowserWidget,   "Libraries");

    QToolButton* refreshToolButton = new QToolButton(tabWidget);
    refreshToolButton->setIcon(QIcon(":/seer/resources/RelaxLightIcons/view-refresh.svg"));
    refreshToolButton->setToolTip("Refresh the source/libraries information.");
    tabWidget->setCornerWidget(refreshToolButton, Qt::TopRightCorner);

    // Connect things.
    QObject::connect(refreshToolButton, &QToolButton::clicked,     this,  &SeerSourceLibraryManagerWidget::handleRefreshToolButtonClicked);
}

SeerSourceLibraryManagerWidget::~SeerSourceLibraryManagerWidget () {
}

SeerSourceBrowserWidget* SeerSourceLibraryManagerWidget::sourceBrowserWidget () {
    return _sourceBrowserWidget;
}

SeerFunctionBrowserWidget* SeerSourceLibraryManagerWidget::functionBrowserWidget () {
    return _functionBrowserWidget;
}

SeerTypeBrowserWidget* SeerSourceLibraryManagerWidget::typeBrowserWidget () {
    return _typeBrowserWidget;
}

SeerVariableBrowserWidget* SeerSourceLibraryManagerWidget::variableBrowserWidget () {
    return _variableBrowserWidget;
}

SeerLibraryBrowserWidget* SeerSourceLibraryManagerWidget::libraryBrowserWidget () {
    return _libraryBrowserWidget;
}

void SeerSourceLibraryManagerWidget::handleRefreshToolButtonClicked () {

    sourceBrowserWidget()->refresh();
    functionBrowserWidget()->refresh();
    typeBrowserWidget()->refresh();
    variableBrowserWidget()->refresh();
    libraryBrowserWidget()->refresh();
}

