#include "SeerSkipBrowserWidget.h"
#include "SeerUtl.h"
#include <QtWidgets/QTreeWidget>
#include <QtWidgets/QTreeWidgetItemIterator>
#include <QtWidgets/QLabel>
#include <QtWidgets/QApplication>
#include <QtCore/QFileInfo>
#include <QtCore/Qt>
#include <QtCore/QMap>
#include <QtCore/QDebug>

SeerSkipBrowserWidget::SeerSkipBrowserWidget (QWidget* parent) : QWidget(parent) {

    // Set the state.
    _id = Seer::createID();

    // Construct the UI.
    setupUi(this);

    // Setup the widgets
    skipTreeWidget->setMouseTracking(true);
    skipTreeWidget->resizeColumnToContents(0);
    skipTreeWidget->resizeColumnToContents(1);
    skipTreeWidget->resizeColumnToContents(2);
    skipTreeWidget->resizeColumnToContents(3);
    skipTreeWidget->resizeColumnToContents(4);
    skipTreeWidget->resizeColumnToContents(5);
    skipTreeWidget->clear();
    skipTreeWidget->setSortingEnabled(false);

    // Connect things.
}

SeerSkipBrowserWidget::~SeerSkipBrowserWidget () {
}

void SeerSkipBrowserWidget::handleText (const QString& text) {

    QApplication::setOverrideCursor(Qt::BusyCursor);

    skipTreeWidget->resizeColumnToContents(0);
    skipTreeWidget->resizeColumnToContents(1);
    skipTreeWidget->resizeColumnToContents(2);
    skipTreeWidget->resizeColumnToContents(3);
    skipTreeWidget->resizeColumnToContents(4);
    skipTreeWidget->resizeColumnToContents(5);

    QApplication::restoreOverrideCursor();
}

void SeerSkipBrowserWidget::refresh () {
}

