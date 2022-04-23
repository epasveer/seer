#include "QDetachTabWidget.h"
#include <QtWidgets/QTabBar>
#include <QtWidgets/QMenu>
#include <QtWidgets/QAction>
#include <QtGui/QCursor>
#include <QtCore/QDebug>

QDetachTabWidget::QDetachTabWidget(QWidget* parent) : QTabWidget(parent) {

    QObject::connect(tabBar(), &QTabBar::tabBarDoubleClicked,           this, &QDetachTabWidget::handleShowContextMenu);
    QObject::connect(this,     &QTabWidget::tabCloseRequested,          this, &QDetachTabWidget::handleTabClosedRequested);
}

void QDetachTabWidget::closeEvent (QCloseEvent* e) {

    auto it = _tabInfo.begin();

    while (it != _tabInfo.end()) {

        delete it->_widget;

        it = _tabInfo.erase(it); // Erase current tab and point to the next one.
    }

    QTabWidget::closeEvent(e);
}

void QDetachTabWidget::handleShowContextMenu(int tabIndex) {

    // Create the menu.
    QMenu menu("Window Action", this);

    QAction* detachAction   = menu.addAction(tr("Detach"));
    QAction* reattachAction = menu.addAction(tr("Reattach"));

    // Enable/disable depending if it was already detached.
    QWidget* w = widget(tabIndex);
    if (w->objectName() == "QDetachTabWidgetPlaceholder") {
        detachAction->setEnabled(false);
        reattachAction->setEnabled(true);
    }else{
        detachAction->setEnabled(true);
        reattachAction->setEnabled(false);
    }

    // Exec the menue
    QAction* action = menu.exec(QCursor::pos());

    //
    // Handle detaching a tab.
    //
    if (action == detachAction) {

        // Get the tab the user selected.
        QWidget* w = widget(tabIndex);

        // It can't be a 'placeholder' as it was already detached.
        if (w->objectName() == "QDetachTabWidgetPlaceholder") {
            return;
        }

        // Create a blank 'placeholder' tab.
        QWidget* placeholder = new QWidget;
        placeholder->setObjectName("QDetachTabWidgetPlaceholder");

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
        w->show();

        // Set the tabwidget to the placeholder tab.
        setCurrentIndex(tabIndex);
    }

    //
    // Handle reattaching a tab.
    //
    if (action == reattachAction) {

        // Get the tab the user selected to re-attach.
        QWidget* w = widget(tabIndex);

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

        // Set the tabwidget to the real tab.
        setCurrentIndex(tabIndex);

        // Delete the entry from the tab cache.
        _tabInfo.erase(it);
    }
}

//
// The QDetachTabWidget listens for 'tabCloseRequests'. Only to
// reattach the widget, if necessary. Normally, you derive from
// QDetachTabWidget and add signal/connect to do the same. I'm
// counting on mine happening first.
//
void QDetachTabWidget::handleTabClosedRequested (int tabIndex) {

    // Get the widget to be closed.
    QWidget* w = widget(tabIndex);

    //qDebug() << "QDetachTabWidget::handleTabClosedRequested:" << tabIndex << w->objectName();

    // If the widget is a 'placeholder', reattach the real widget and delete the 'placeholder'.
    if (w->objectName() == "QDetachTabWidgetPlaceholder") {

        //qDebug() << "Reattaching detached tab.";

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

        // Set the tabwidget to the real tab.
        setCurrentIndex(tabIndex);

        // Delete the entry from the tab cache.
        _tabInfo.erase(it);
    }
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

