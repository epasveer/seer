#include "SeerThreadManagerWidget.h"
#include <QtWidgets/QToolButton>
#include <QtGui/QIcon>
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

    tabWidget->addTab(_threadFramesBrowserWidget, "Frames");
    tabWidget->addTab(_threadIdsBrowserWidget,    "Ids");

    QToolButton* refreshToolButton = new QToolButton(tabWidget);
    refreshToolButton->setIcon(QIcon(":/seer/resources/RelaxLightIcons/view-refresh.svg"));
    refreshToolButton->setToolTip("Refresh the thread information.");
    tabWidget->setCornerWidget(refreshToolButton, Qt::TopRightCorner);

    // Connect things.
    QObject::connect(refreshToolButton, &QToolButton::clicked,     this,  &SeerThreadManagerWidget::handleRefreshToolButtonClicked);
}

SeerThreadManagerWidget::~SeerThreadManagerWidget () {
}

SeerThreadIdsBrowserWidget* SeerThreadManagerWidget::threadIdsBrowserWidget () {
    return _threadIdsBrowserWidget;
}

SeerThreadFramesBrowserWidget* SeerThreadManagerWidget::threadFramesBrowserWidget () {
    return _threadFramesBrowserWidget;
}

void SeerThreadManagerWidget::handleRefreshToolButtonClicked () {

    threadFramesBrowserWidget()->refresh();
    threadIdsBrowserWidget()->refresh();
}

