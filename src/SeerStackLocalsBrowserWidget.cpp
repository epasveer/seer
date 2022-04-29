#include "SeerStackLocalsBrowserWidget.h"
#include "SeerUtl.h"
#include <QtWidgets/QTreeWidget>
#include <QtWidgets/QTreeWidgetItemIterator>
#include <QtWidgets/QApplication>
#include <QtCore/QDebug>

SeerStackLocalsBrowserWidget::SeerStackLocalsBrowserWidget (QWidget* parent) : QWidget(parent) {

    // Construct the UI.
    setupUi(this);

    // Setup the widgets
    localsTreeWidget->setMouseTracking(true);
    localsTreeWidget->setSortingEnabled(false);
    localsTreeWidget->resizeColumnToContents(0); // name
    localsTreeWidget->resizeColumnToContents(1); // arg
    localsTreeWidget->resizeColumnToContents(2); // value
    localsTreeWidget->resizeColumnToContents(3); // used

    localsTreeWidget->setColumnHidden(3, true); // Hide the 'used' column.

    localsTreeWidget->clear();

    _frameNumber = 0;

    // Connect things.
    QObject::connect(localsTreeWidget, &QTreeWidget::itemCollapsed,    this,  &SeerStackLocalsBrowserWidget::handleItemCollapsed);
    QObject::connect(localsTreeWidget, &QTreeWidget::itemExpanded,     this,  &SeerStackLocalsBrowserWidget::handleItemExpanded);
    QObject::connect(localsTreeWidget, &QTreeWidget::itemEntered,      this,  &SeerStackLocalsBrowserWidget::handleItemEntered);
}

SeerStackLocalsBrowserWidget::~SeerStackLocalsBrowserWidget () {
}

void SeerStackLocalsBrowserWidget::handleText (const QString& text) {

    // Don't do any work if the widget is hidden.
    if (isHidden()) {
        return;
    }

    QApplication::setOverrideCursor(Qt::BusyCursor);

    if (text.startsWith("^done,variables=[") && text.endsWith("]")) {

        // Mark each entry initially as "unused".
        // Later, some will be marked as "reused" or "new". Then the "unused" ones will
        // be deleted.
        QTreeWidgetItemIterator it(localsTreeWidget);
        while (*it) {
            (*it)->setText(3, "unused");
            ++it;
        }

        // ^done,variables=[
        //     {name=\"message\",arg=\"1\",value=\"\\\"Hello, World!\\\"\"},
        //     {name=\"something\",arg=\"1\",value=\"\\\"Hello, World!\\\"\"}
        // ]

        //qDebug() << text;

        QString frame_text = Seer::parseFirst(text, "variables=", '[', ']', false);

        QStringList variable_list = Seer::parse(frame_text, "", '{', '}', false);

        for ( const auto& variable_text : variable_list  ) {

            QString name_text  = Seer::parseFirst(variable_text, "name=",  '"', '"', false);
            QString arg_text   = Seer::parseFirst(variable_text, "arg=",   '"', '"', false);
            QString value_text = Seer::parseFirst(variable_text, "value=", '"', '"', false);

            // Instead of creating a new tree each time, we will reuse existing items, if they are there.
            // This allows the expanded items to remain expanded.
            QList<QTreeWidgetItem*> matches = localsTreeWidget->findItems(name_text, Qt::MatchExactly, 0);

            // No matches. So can't reuse. Add the new entry.
            if (matches.size() == 0) {

                // Add a complex entry to the tree.
                if (value_text.startsWith("{") && value_text.endsWith("}")) {

                    QTreeWidgetItem* topItem = new QTreeWidgetItem;
                    topItem->setText(0, name_text);
                    topItem->setText(1, "");
                    topItem->setText(2, "");
                    topItem->setText(3, "new");

                    localsTreeWidget->addTopLevelItem(topItem);

                    QTreeWidgetItem* item = new QTreeWidgetItem;
                    item->setText(0, name_text);
                    item->setText(1, arg_text);
                    item->setText(2, Seer::filterEscapes(value_text));
                    item->setText(3, "new");

                    topItem->addChild(item);

                // Add the simple entry to the tree.
                }else{
                    QTreeWidgetItem* item = new QTreeWidgetItem;
                    item->setText(0, name_text);
                    item->setText(1, arg_text);
                    item->setText(2, Seer::filterEscapes(value_text));
                    item->setText(3, "new");

                    localsTreeWidget->addTopLevelItem(item);
                }

            // Found a match. Reuse it.
            }else{

                QTreeWidgetItem* item = matches.takeFirst();

                // Add a complex entry to the tree.
                if (value_text.startsWith("{") && value_text.endsWith("}")) {
                    // Complex entries have a child. Reuse one or create one.
                    QTreeWidgetItem* child;

                    if (item->childCount() > 0) {
                        child = item->child(0);

                    }else{
                        child = new QTreeWidgetItem;
                        item->addChild(item);
                    }

                    item->setText(0, name_text);
                    item->setText(1, "");
                    item->setText(2, "");
                    item->setText(3, "reused");

                    child->setText(0, name_text);
                    child->setText(1, arg_text);
                    child->setText(2, Seer::filterEscapes(value_text));
                    child->setText(3, "reused");

                // Add the simple entry to the tree.
                }else{
                    // Simple entries don't have children. Delete them.
                    if (item->childCount() > 0) {
                        QList<QTreeWidgetItem*> children = item->takeChildren();

                        qDeleteAll(children);
                    }

                    item->setText(0, name_text);
                    item->setText(1, arg_text);
                    item->setText(2, Seer::filterEscapes(value_text));
                    item->setText(3, "reused");
                }
            }
        }

        // At this point, there are some new entries, some reused entries, and some unused ones.
        // Delete the unused ones. They are obsolete.
        QList<QTreeWidgetItem*> matches = localsTreeWidget->findItems("unused", Qt::MatchExactly, 3);

        qDeleteAll(matches);

    }else if (text.startsWith("^error,msg=\"No registers.\"")) {
        localsTreeWidget->clear();

    }else{
        // Ignore others.
    }

    localsTreeWidget->resizeColumnToContents(0);
    localsTreeWidget->resizeColumnToContents(1);
    localsTreeWidget->resizeColumnToContents(2);
    localsTreeWidget->resizeColumnToContents(3);

    QApplication::restoreOverrideCursor();
}

void SeerStackLocalsBrowserWidget::handleStoppingPointReached () {

    // Don't do any work if the widget is hidden.
    if (isHidden()) {
        return;
    }

    refresh();
}

void SeerStackLocalsBrowserWidget::refresh () {
    emit refreshStackLocals();
}

void SeerStackLocalsBrowserWidget::handleItemExpanded (QTreeWidgetItem* item) {

    Q_UNUSED(item);

    localsTreeWidget->resizeColumnToContents(0);
    localsTreeWidget->resizeColumnToContents(1);
    localsTreeWidget->resizeColumnToContents(2);
    localsTreeWidget->resizeColumnToContents(3);
}

void SeerStackLocalsBrowserWidget::handleItemCollapsed (QTreeWidgetItem* item) {

    Q_UNUSED(item);

    localsTreeWidget->resizeColumnToContents(0);
    localsTreeWidget->resizeColumnToContents(1);
    localsTreeWidget->resizeColumnToContents(2);
    localsTreeWidget->resizeColumnToContents(3);
}

void SeerStackLocalsBrowserWidget::handleItemEntered (QTreeWidgetItem* item, int column) {

    Q_UNUSED(column);

    //qDebug() << item->text(0) << column;

    item->setToolTip(0, item->text(0) + " : " + item->text(2));

    for (int i=1; i<localsTreeWidget->columnCount(); i++) { // Copy tooltip to the other columns.
        item->setToolTip(i, item->toolTip(0));
    }
}

void SeerStackLocalsBrowserWidget::showEvent (QShowEvent* event) {

    QWidget::showEvent(event);

    refresh();
}

