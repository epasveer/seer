#include "SeerStackManagerWidget.h"
#include "SeerHelpPageDialog.h"
#include "SeerUtl.h"
#include "QHContainerWidget.h"
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

    // Connect things.
    QObject::connect(refreshToolButton,  &QToolButton::clicked,                          this,  &SeerStackManagerWidget::handleRefreshToolButtonClicked);
    QObject::connect(helpToolButton,     &QToolButton::clicked,                          this,  &SeerStackManagerWidget::handleHelpToolButtonClicked);
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

        groupBox->setTitle("Stack Info for Thread Id : " + currentthreadid_text);

    }else if (text.startsWith("^error,msg=\"No registers.\"")) {

        groupBox->setTitle("Stack Info");

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


