#include "QDetachTabWidget.h"
#include <QtWidgets/QTabBar>
#include <QtWidgets/QMenu>
#include <QtWidgets/QHBoxLayout>
#include <QAction>
#include <QtGui/QCursor>
#include <QtGui/QIcon>
#include <QtCore/QTimer>
#include <QtCore/QDebug>

QDetachTabWidget::QDetachTabWidget(QWidget* parent) : QTabWidget(parent) {

    tabBar()->setContextMenuPolicy(Qt::CustomContextMenu);

    QObject::connect(tabBar(), &QTabBar::customContextMenuRequested,    this, &QDetachTabWidget::handleShowContextMenu);
    QObject::connect(this,     &QTabWidget::tabCloseRequested,          this, &QDetachTabWidget::handleTabClosedRequested);
}

bool QDetachTabWidget::isDetached (int tabIndex) const {

    // Get the tab the user selected.
    QWidget* w = widget(tabIndex);

    if (w == nullptr) {
        return false;
    }

    // If it's the 'placeholder' it is detached.
    if (w->objectName() == "QDetachTabWidgetPlaceholder") {
        return true;
    }

    // Otherwise, it's still attached.
    return false;
}

QWidget* QDetachTabWidget::tabWidget (int tabIndex) const {

    if (tabIndex < 0 || tabIndex > _tabInfo.size()) {
        return nullptr;
    }

    return _tabInfo[tabIndex]._widget;
}

void QDetachTabWidget::detachTab (int tabIndex, bool minimized) {

    // Get the tab the user selected.
    QWidget* w = widget(tabIndex);

    if (w == nullptr) {
        return;
    }

    // It can't be a 'placeholder' as it was already detached.
    if (w->objectName() == "QDetachTabWidgetPlaceholder") {
        return;
    }

    // Create a blank 'placeholder' tab.
    QDetachTabWidgetPlaceholder* placeholder = new QDetachTabWidgetPlaceholder(tabIndex);

    // Create an entry for the tab cache. Remember the real tab and the 'placeholder' tab. Plus the tab title.
    QDetachTabInfo tabinfo(tabText(tabIndex), w, placeholder);
    _tabInfo.append(tabinfo);

    // Remove the real tab.
    removeTab(tabIndex);

    // Insert the 'placeholder' in the same position.
    insertTab(tabIndex, placeholder, tabinfo._title);

    // Tweak the detached tab to make sure it can't be destroyed as it is now a normal window.
    w->setParent(0);
    Qt::WindowFlags flags     = w->windowFlags();
    Qt::WindowFlags closeFlag = Qt::WindowCloseButtonHint;
    flags = flags & (~closeFlag);
    w->setWindowFlags(flags);
    w->setWindowTitle(tabinfo._title);
    w->setWindowIcon(windowIcon());

    if (minimized) {
        w->showMinimized();
    }else{
        w->showNormal();
    }

    // Connect the placeholder's 'reattach' signal to the slot.
    QObject::connect(placeholder, &QDetachTabWidgetPlaceholder::reattach,   this, &QDetachTabWidget::handleTabClosedRequested);

    // Notify listeners the tab was detached.
    emit tabDetached(tabIndex);
    emit tabDetached(w);
}

void QDetachTabWidget::reattachTab (int tabIndex) {

    // Get the tab the user selected to re-attach.
    QWidget* w = widget(tabIndex);

    if (w == nullptr) {
        return;
    }

    // It must be a 'placeholder'
    if (w->objectName() != "QDetachTabWidgetPlaceholder") {
        return;
    }

    // Look for the 'placeholder' tab in the tab cache.
    // Note, we do a scan of the cache because a QTabWidget can have isMovable() enabled.
    // Using 'tabindex' is not always reliable.
    QList<QDetachTabInfo>::iterator it = findPlaceholderWidget(w);
    if (it == _tabInfo.end()) {
        return;
    }

    // Remove the 'placeholder' tab.
    removeTab(tabIndex); delete w;

    // Insert the real tab in the same position.
    insertTab(tabIndex, it->_widget, it->_title);

    // Save the real tab's widget before we delete the entry.
    w = it->_widget;

    // Delete the entry from the tab cache.
    _tabInfo.erase(it);

    // Notify listeners the tab was reattached.
    emit tabReattached(tabIndex);
    emit tabReattached(w);
}

void QDetachTabWidget::closeEvent (QCloseEvent* e) {

    auto it = _tabInfo.begin();

    while (it != _tabInfo.end()) {

        delete it->_widget;

        it = _tabInfo.erase(it); // Erase current tab and point to the next one.
    }

    QTabWidget::closeEvent(e);
}

void QDetachTabWidget::handleShowContextMenu (const QPoint& point) {

    // Don't do anything.
    if (point.isNull()) {
        return;
    }

    // Get the tab index at the RMB click point.
    int tabIndex = tabBar()->tabAt(point);

    // Create the menu.
    QMenu menu("Window Action", this);

    QAction* detachAction          = menu.addAction(tr("Detach"));
    QAction* detachMinimizedAction = menu.addAction(tr("Detach Minimized"));
    QAction* reattachAction        = menu.addAction(tr("Reattach"));

    // Enable/disable depending if it was already detached.
    QWidget* w = widget(tabIndex);
    if (w->objectName() == "QDetachTabWidgetPlaceholder") {
        detachAction->setEnabled(false);
        detachMinimizedAction->setEnabled(false);
        reattachAction->setEnabled(true);
    }else{
        detachAction->setEnabled(true);
        detachMinimizedAction->setEnabled(true);
        reattachAction->setEnabled(false);
    }

    // Exec the menue
    QAction* action = menu.exec(QCursor::pos());

    //
    // Handle detaching a tab.
    //
    if (action == detachAction) {

        // Detach the tab.
        detachTab(tabIndex, false);

        // Set the tabwidget to the placeholder tab.
        setCurrentIndex(tabIndex);
    }

    //
    // Handle detaching a tab.
    //
    if (action == detachMinimizedAction) {

        // Detach the tab.
        detachTab(tabIndex, true);

        // Set the tabwidget to the placeholder tab.
        setCurrentIndex(tabIndex);
    }

    //
    // Handle reattaching a tab.
    //
    if (action == reattachAction) {

        // Reattach the tab.
        reattachTab(tabIndex);

        // Set the tabwidget to the real tab.
        setCurrentIndex(tabIndex);
    }
}

//
// The QDetachTabWidget listens for 'tabCloseRequests'. Only to
// reattach the widget, if necessary. Normally, you derive from
// QDetachTabWidget and add signal/connect to do the same. I'm
// counting on mine happening first.
//
void QDetachTabWidget::handleTabClosedRequested (int tabIndex) {

    // Reattach the tab.
    reattachTab(tabIndex);

    // Set the tabwidget to the real tab.
    setCurrentIndex(tabIndex);
}

//
// Look for the real widget in the tab cache.
//
QList<QDetachTabInfo>::iterator QDetachTabWidget::findWidget (QWidget* widget) {

    QList<QDetachTabInfo>::iterator it = _tabInfo.begin();

    while (it != _tabInfo.end()) {
        if (it->_widget == widget) {
            return it;
        }

        it++;
    }

    return it;
}

//
// Look for the 'placeholder' widget in the tab cache.
//
QList<QDetachTabInfo>::iterator QDetachTabWidget::findPlaceholderWidget (QWidget* widget) {

    QList<QDetachTabInfo>::iterator it = _tabInfo.begin();

    while (it != _tabInfo.end()) {
        if (it->_placeholderWidget == widget) {
            return it;
        }

        it++;
    }

    return it;
}

//
// A placeholder widget that has a 'reattach' pushbutton.
//
QDetachTabWidgetPlaceholder::QDetachTabWidgetPlaceholder(int tabIndex, QWidget* parent) : QWidget(parent) {

    // This the placeholder's object name. Users of this object can focus
    // on this name if they need too.
    setObjectName("QDetachTabWidgetPlaceholder");

    // Create the 'Reattach' button and add a layout to it.
    _reattachPushButton = new QPushButton("Reattach", this);
    _reattachPushButton->setIcon(QIcon(":/qt-project.org/styles/commonstyle/images/right-16.png"));
    _tabIndex           = tabIndex;

    QHBoxLayout* layout = new QHBoxLayout;
    layout->addStretch();
    layout->addWidget(_reattachPushButton);
    layout->addStretch();

    setLayout(layout);

    // We'll handle the 'click' signal.
    QObject::connect(_reattachPushButton, &QPushButton::clicked,    this, &QDetachTabWidgetPlaceholder::handlePushButtonClicked);
}

// Handle the pushbutton 'click' by emitting the 'reattach' signal.
void QDetachTabWidgetPlaceholder::handlePushButtonClicked () {
    emit reattach(_tabIndex);
}

