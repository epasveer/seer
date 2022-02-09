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

    _sourceBrowserWidget        = new SeerSourceBrowserWidget(this);
    _sharedLibraryBrowserWidget = new SeerSharedLibraryBrowserWidget(this);

    tabWidget->addTab(_sourceBrowserWidget,        "Source");
    tabWidget->addTab(_sharedLibraryBrowserWidget, "Libraries");

    QToolButton* refreshToolButton = new QToolButton(tabWidget);
    refreshToolButton->setIcon(QIcon(":/seer/resources/HighContrast/view-refresh.png"));
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

SeerSharedLibraryBrowserWidget* SeerSourceLibraryManagerWidget::sharedLibraryBrowserWidget () {
    return _sharedLibraryBrowserWidget;
}

void SeerSourceLibraryManagerWidget::handleRefreshToolButtonClicked () {

    sourceBrowserWidget()->refresh();
    sharedLibraryBrowserWidget()->refresh();
}

