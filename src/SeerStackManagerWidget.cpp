#include "SeerStackManagerWidget.h"
#include <QtWidgets/QToolButton>
#include <QtGui/QIcon>
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
    refreshToolButton->setIcon(QIcon::fromTheme("view-refresh"));
    refreshToolButton->setToolTip("Refresh the stack information.");
    tabWidget->setCornerWidget(refreshToolButton, Qt::TopRightCorner);

    // Connect things.
    QObject::connect(refreshToolButton,         &QToolButton::clicked,                          this,                      &SeerStackManagerWidget::handleRefreshToolButtonClicked);
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
}

