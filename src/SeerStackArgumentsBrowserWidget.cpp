// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "SeerStackArgumentsBrowserWidget.h"
#include "SeerUtl.h"
#include <QtWidgets/QTreeWidget>
#include <QtWidgets/QTreeWidgetItemIterator>
#include <QtWidgets/QApplication>
#include <QtWidgets/QMenu>
#include <QtGui/QFontDatabase>
#include <QAction>
#include <QtCore/QDebug>

SeerStackArgumentsBrowserWidget::SeerStackArgumentsBrowserWidget (QWidget* parent) : QWidget(parent) {

    // Construct the UI.
    setupUi(this);

    // Setup the widgets
    argumentsTreeWidget->setContextMenuPolicy(Qt::CustomContextMenu);
    argumentsTreeWidget->setMouseTracking(true);
    argumentsTreeWidget->setSortingEnabled(false);
    argumentsTreeWidget->resizeColumnToContents(0); // level
    argumentsTreeWidget->resizeColumnToContents(1); // Name
    argumentsTreeWidget->resizeColumnToContents(2); // Value
    argumentsTreeWidget->resizeColumnToContents(3); // used
    argumentsTreeWidget->setColumnHidden(3, true);  // Hide the 'used' column.
    argumentsTreeWidget->clear();

    // Connect things.
    QObject::connect(argumentsTreeWidget, &QTreeWidget::customContextMenuRequested,    this,  &SeerStackArgumentsBrowserWidget::handleContextMenu);
    QObject::connect(argumentsTreeWidget, &QTreeWidget::itemCollapsed,                 this,  &SeerStackArgumentsBrowserWidget::handleItemCollapsed);
    QObject::connect(argumentsTreeWidget, &QTreeWidget::itemExpanded,                  this,  &SeerStackArgumentsBrowserWidget::handleItemExpanded);
    QObject::connect(argumentsTreeWidget, &QTreeWidget::itemEntered,                   this,  &SeerStackArgumentsBrowserWidget::handleItemEntered);
}

SeerStackArgumentsBrowserWidget::~SeerStackArgumentsBrowserWidget () {
}

void SeerStackArgumentsBrowserWidget::handleText (const QString& text) {

    // Don't do any work if the widget is hidden.
    if (isHidden()) {
        return;
    }

    QApplication::setOverrideCursor(Qt::BusyCursor);

    if (text.startsWith("^done,stack-args=[") && text.endsWith("]")) {

        //argumentsTreeWidget->clear();

        // ^done,stack-args=[
        //     frame={level="0",args=[
        //                            {name="message",value="\"Hello, World!\""}
        //                           ]},
        //     frame={level="1",args=[
        //                            {name="argc",value="1"},
        //                            {name="argv",value="0x7fffffffd5b8"}
        //                           ]}
        // ]

        // "level=\"0\",args=[{name=\"message\",value=\"\\\"Hello, World!\\\"\"}]",
        // "level=\"1\",args=[{name=\"argc\",value=\"1\"},{name=\"argv\",value=\"0x7fffffffd5b8\"}]"

        QStringList frame_list = Seer::parse(text, "frame=", '{', '}', false);

        // Mark each entry initially as "unused".
        // Later, some will be marked as "reused" or "new". Then the "unused" ones will
        // be deleted.
        QTreeWidgetItemIterator it(argumentsTreeWidget);
        while (*it) {
            (*it)->setText(3, "unused");
            ++it;
        }

        for ( const auto& frame_text : frame_list  ) {

            QString level_text = Seer::parseFirst(frame_text, "level=", '"', '"', false);
            QString args_text  = Seer::parseFirst(frame_text, "args=",  '[', ']', false);

            QStringList namevalue_list  = Seer::parse(args_text, "",  '{', '}', false);

            QList<QTreeWidgetItem*> matches = argumentsTreeWidget->findItems(level_text, Qt::MatchExactly, 0);

            QTreeWidgetItem* topItem = 0;

            // Use an existing level.
            if (matches.count() > 0) {
                topItem = matches[0];
                topItem->setText(3, "reused");

            // Add the new level to the tree.
            }else{
                topItem = new QTreeWidgetItem;
                topItem->setText(0, level_text);
                topItem->setText(3, "new");

                argumentsTreeWidget->addTopLevelItem(topItem);
            }

            // Get the argument names and values for the level.
            for ( const auto& namevalue_text : namevalue_list  ) {

                QString name_text  = Seer::parseFirst(namevalue_text, "name=",  '"', '"', false);
                QString value_text = Seer::parseFirst(namevalue_text, "value=", '"', '"', false);

                // Populate the tree.
                handleItemCreate(topItem, "", name_text, value_text);
            }

            // Expand all items for the level.
            argumentsTreeWidget->expandItem(topItem);
        }

        // At this point, there are some new entries, some reused entries, and some unused ones.
        // Delete the unused ones. They are obsolete.
        // Don't use qDeleteAll() here. It doesn't work as expected for items that are "found".
        // Instead, get a list of matches and delete them from the bottom up.
        QList<QTreeWidgetItem*> matches = argumentsTreeWidget->findItems("unused", Qt::MatchExactly|Qt::MatchRecursive, 3);

        while (matches.isEmpty() == false) {
            foreach (QTreeWidgetItem* item, matches) {
                if (item->childCount() == 0) {
                    QTreeWidgetItem* parent = item->parent();
                    if (parent) {
                        parent->removeChild(item);
                    }

                    bool f = matches.removeOne(item);
                    Q_ASSERT(f != false);

                    delete item;

                    break;
                }
            }
        }

    }else{
        // Ignore others.
    }

    argumentsTreeWidget->resizeColumnToContents(0);
    argumentsTreeWidget->resizeColumnToContents(1);
    argumentsTreeWidget->resizeColumnToContents(2);
    argumentsTreeWidget->resizeColumnToContents(3);

    QApplication::restoreOverrideCursor();
}

void SeerStackArgumentsBrowserWidget::handleStoppingPointReached () {

    refresh();
}

void SeerStackArgumentsBrowserWidget::handleSessionTerminated () {

    // Delete previous contents.
    argumentsTreeWidget->clear();
}

void SeerStackArgumentsBrowserWidget::refresh () {

    // Don't do any work if the widget is hidden.
    if (isHidden()) {
        return;
    }

    emit refreshStackArguments();
}

void SeerStackArgumentsBrowserWidget::handleContextMenu (const QPoint& pos) {

    QTreeWidgetItem* item = argumentsTreeWidget->itemAt(pos);

    if (item == 0) {
        return;
    }

    if (item->text(1) == "") {
        return;
    }

    QAction* addVariableLoggerExpressionAction;
    QAction* addVariableLoggerAsteriskExpressionAction;
    QAction* addVariableLoggerAmpersandExpressionAction;
    QAction* addVariableLoggerAsteriskAmpersandExpressionAction;
    QAction* addVariableLoggerObjcExpressionAction;
    QAction* addVariableTrackerExpressionAction;
    QAction* addVariableTrackerAsteriskExpressionAction;
    QAction* addVariableTrackerAmpersandExpressionAction;
    QAction* addVariableTrackerAsteriskAmpersandExpressionAction;
    QAction* addVariableTrackerObjcExpressionAction;
    QAction* addMemoryVisualizerAction;
    QAction* addMemoryAsteriskVisualizerAction;
    QAction* addMemoryAmpersandVisualizerAction;
    QAction* addArrayVisualizerAction;
    QAction* addArrayAsteriskVisualizerAction;
    QAction* addArrayAmpersandVisualizerAction;
    QAction* addMatrixVisualizerAction;
    QAction* addMatrixAsteriskVisualizerAction;
    QAction* addMatrixAmpersandVisualizerAction;
    QAction* addStructVisualizerAction;
    QAction* addStructAsteriskVisualizerAction;
    QAction* addStructAmpersandVisualizerAction;

    addVariableLoggerExpressionAction                   = new QAction(QString("\"%1\"").arg(item->text(1)));
    addVariableLoggerAsteriskExpressionAction           = new QAction(QString("\"*%1\"").arg(item->text(1)));
    addVariableLoggerAmpersandExpressionAction          = new QAction(QString("\"&&%1\"").arg(item->text(1)));
    addVariableLoggerAsteriskAmpersandExpressionAction  = new QAction(QString("\"*&&%1\"").arg(item->text(1)));
    addVariableLoggerObjcExpressionAction               = new QAction(QString("\"(objc)%1\"").arg(item->text(0)));
    addVariableTrackerExpressionAction                  = new QAction(QString("\"%1\"").arg(item->text(1)));
    addVariableTrackerAsteriskExpressionAction          = new QAction(QString("\"*%1\"").arg(item->text(1)));
    addVariableTrackerAmpersandExpressionAction         = new QAction(QString("\"&&%1\"").arg(item->text(1)));
    addVariableTrackerAsteriskAmpersandExpressionAction = new QAction(QString("\"*&&%1\"").arg(item->text(1)));
    addVariableTrackerObjcExpressionAction              = new QAction(QString("\"(objc)%1\"").arg(item->text(0)));
    addMemoryVisualizerAction                           = new QAction(QString("\"%1\"").arg(item->text(1)));
    addMemoryAsteriskVisualizerAction                   = new QAction(QString("\"*%1\"").arg(item->text(1)));
    addMemoryAmpersandVisualizerAction                  = new QAction(QString("\"&&%1\"").arg(item->text(1)));
    addArrayVisualizerAction                            = new QAction(QString("\"%1\"").arg(item->text(1)));
    addArrayAsteriskVisualizerAction                    = new QAction(QString("\"*%1\"").arg(item->text(1)));
    addArrayAmpersandVisualizerAction                   = new QAction(QString("\"&&%1\"").arg(item->text(1)));
    addMatrixVisualizerAction                           = new QAction(QString("\"%1\"").arg(item->text(1)));
    addMatrixAsteriskVisualizerAction                   = new QAction(QString("\"*%1\"").arg(item->text(1)));
    addMatrixAmpersandVisualizerAction                  = new QAction(QString("\"&&%1\"").arg(item->text(1)));
    addStructVisualizerAction                           = new QAction(QString("\"%1\"").arg(item->text(1)));
    addStructAsteriskVisualizerAction                   = new QAction(QString("\"*%1\"").arg(item->text(1)));
    addStructAmpersandVisualizerAction                  = new QAction(QString("\"&&%1\"").arg(item->text(1)));

    QMenu menu("Visualizers", this);
    menu.setTitle("Visualizers");

    QMenu loggerMenu("Add variable to Logger");
    loggerMenu.addAction(addVariableLoggerExpressionAction);
    loggerMenu.addAction(addVariableLoggerAsteriskExpressionAction);
    loggerMenu.addAction(addVariableLoggerAmpersandExpressionAction);
    loggerMenu.addAction(addVariableLoggerAsteriskAmpersandExpressionAction);
    loggerMenu.addAction(addVariableLoggerObjcExpressionAction);
    menu.addMenu(&loggerMenu);

    QMenu trackerMenu("Add variable to Tracker");
    trackerMenu.addAction(addVariableTrackerExpressionAction);
    trackerMenu.addAction(addVariableTrackerAsteriskExpressionAction);
    trackerMenu.addAction(addVariableTrackerAmpersandExpressionAction);
    trackerMenu.addAction(addVariableTrackerAsteriskAmpersandExpressionAction);
    trackerMenu.addAction(addVariableTrackerObjcExpressionAction);
    menu.addMenu(&trackerMenu);

    QMenu memoryVisualizerMenu("Add variable to a Memory Visualizer");
    memoryVisualizerMenu.addAction(addMemoryVisualizerAction);
    memoryVisualizerMenu.addAction(addMemoryAsteriskVisualizerAction);
    memoryVisualizerMenu.addAction(addMemoryAmpersandVisualizerAction);
    menu.addMenu(&memoryVisualizerMenu);

    QMenu arrayVisualizerMenu("Add variable to an Array Visualizer");
    arrayVisualizerMenu.addAction(addArrayVisualizerAction);
    arrayVisualizerMenu.addAction(addArrayAsteriskVisualizerAction);
    arrayVisualizerMenu.addAction(addArrayAmpersandVisualizerAction);
    menu.addMenu(&arrayVisualizerMenu);

    QMenu matrixVisualizerMenu("Add variable to a Matrix Visualizer");
    matrixVisualizerMenu.addAction(addMatrixVisualizerAction);
    matrixVisualizerMenu.addAction(addMatrixAsteriskVisualizerAction);
    matrixVisualizerMenu.addAction(addMatrixAmpersandVisualizerAction);
    menu.addMenu(&matrixVisualizerMenu);

    QMenu structVisualizerMenu("Add variable to a Struct Visualizer");
    structVisualizerMenu.addAction(addStructVisualizerAction);
    structVisualizerMenu.addAction(addStructAsteriskVisualizerAction);
    structVisualizerMenu.addAction(addStructAmpersandVisualizerAction);
    menu.addMenu(&structVisualizerMenu);

    // Launch the menu. Get the response.
    QAction* action = menu.exec(argumentsTreeWidget->viewport()->mapToGlobal(pos));

    // Do nothing.
    if (action == 0) {
        return;
    }

    // Handle adding a variable to log.
    if (action == addVariableLoggerExpressionAction) {

        // Emit the signals.
        if (item->text(1) != "") {
            emit addVariableLoggerExpression(item->text(1));
        }

        return;
    }

    // Handle adding a variable to log.
    if (action == addVariableLoggerAsteriskExpressionAction) {

        // Emit the signals.
        if (item->text(1) != "") {
            emit addVariableLoggerExpression(QString("*") + item->text(1));
        }

        return;
    }

    // Handle adding a variable to log.
    if (action == addVariableLoggerAmpersandExpressionAction) {

        // Emit the signals.
        if (item->text(1) != "") {
            emit addVariableLoggerExpression(QString("&") + item->text(1));
        }

        return;
    }

    // Handle adding a variable to log.
    if (action == addVariableLoggerAsteriskAmpersandExpressionAction) {

        // Emit the signals.
        if (item->text(1) != "") {
            emit addVariableLoggerExpression(QString("*&") + item->text(1));
        }

        return;
    }

    // Handle adding a variable to log.
    if (action == addVariableLoggerObjcExpressionAction) {

        // Emit the signals.
        if (item->text(1) != "") {
            emit addVariableLoggerExpression(QString("(objc)") + item->text(1));
        }

        return;
    }

    // Handle adding a variable to track.
    if (action == addVariableTrackerExpressionAction) {

        // Emit the signals.
        if (item->text(1) != "") {
            emit addVariableTrackerExpression(item->text(1));
            emit refreshVariableTrackerValues();
        }

        return;
    }

    // Handle adding a variable to track.
    if (action == addVariableTrackerAsteriskExpressionAction) {

        // Emit the signals.
        if (item->text(1) != "") {
            emit addVariableTrackerExpression(QString("*") + item->text(1));
            emit refreshVariableTrackerValues();
        }

        return;
    }

    // Handle adding a variable to track.
    if (action == addVariableTrackerAmpersandExpressionAction) {

        // Emit the signals.
        if (item->text(1) != "") {
            emit addVariableTrackerExpression(QString("&") + item->text(1));
            emit refreshVariableTrackerValues();
        }

        return;
    }

    // Handle adding a variable to track.
    if (action == addVariableTrackerAsteriskAmpersandExpressionAction) {

        // Emit the signals.
        if (item->text(1) != "") {
            emit addVariableTrackerExpression(QString("*&") + item->text(1));
            emit refreshVariableTrackerValues();
        }

        return;
    }

    // Handle adding a variable to track.
    if (action == addVariableTrackerObjcExpressionAction) {

        // Emit the signals.
        if (item->text(1) != "") {
            emit addVariableTrackerExpression(QString("(objc)") + item->text(1));
            emit refreshVariableTrackerValues();
        }

        return;
    }

    // Handle adding memory to visualize.
    if (action == addMemoryVisualizerAction) {

        // Emit the signals.
        if (item->text(1) != "") {
            emit addMemoryVisualizer(item->text(1));
        }

        return;
    }

    // Handle adding memory to visualize.
    if (action == addMemoryAsteriskVisualizerAction) {

        // Emit the signals.
        if (item->text(1) != "") {
            emit addMemoryVisualizer(QString("*") + item->text(1));
        }

        return;
    }

    // Handle adding memory to visualize.
    if (action == addMemoryAmpersandVisualizerAction) {

        // Emit the signals.
        if (item->text(1) != "") {
            emit addMemoryVisualizer(QString("&") + item->text(1));
        }

        return;
    }

    // Handle adding array to visualize.
    if (action == addArrayVisualizerAction) {

        // Emit the signals.
        if (item->text(1) != "") {
            emit addArrayVisualizer(item->text(1));
        }

        return;
    }

    // Handle adding array to visualize.
    if (action == addArrayAsteriskVisualizerAction) {

        // Emit the signals.
        if (item->text(1) != "") {
            emit addArrayVisualizer(QString("*") + item->text(1));
        }

        return;
    }

    // Handle adding array to visualize.
    if (action == addArrayAmpersandVisualizerAction) {

        // Emit the signals.
        if (item->text(1) != "") {
            emit addArrayVisualizer(QString("&") + item->text(1));
        }

        return;
    }

    // Handle adding matrix to visualize.
    if (action == addMatrixVisualizerAction) {

        // Emit the signals.
        if (item->text(1) != "") {
            emit addMatrixVisualizer(item->text(1));
        }

        return;
    }

    // Handle adding matrix to visualize.
    if (action == addMatrixAsteriskVisualizerAction) {

        // Emit the signals.
        if (item->text(1) != "") {
            emit addMatrixVisualizer(QString("*") + item->text(1));
        }

        return;
    }

    // Handle adding matrix to visualize.
    if (action == addMatrixAmpersandVisualizerAction) {

        // Emit the signals.
        if (item->text(1) != "") {
            emit addMatrixVisualizer(QString("&") + item->text(1));
        }

        return;
    }

    // Handle adding struct to visualize.
    if (action == addStructVisualizerAction) {

        // Emit the signals.
        if (item->text(1) != "") {
            emit addStructVisualizer(item->text(1));
        }

        return;
    }

    // Handle adding struct to visualize.
    if (action == addStructAsteriskVisualizerAction) {

        // Emit the signals.
        if (item->text(1) != "") {
            emit addStructVisualizer(QString("*") + item->text(1));
        }

        return;
    }

    // Handle adding struct to visualize.
    if (action == addStructAmpersandVisualizerAction) {

        // Emit the signals.
        if (item->text(1) != "") {
            emit addStructVisualizer(QString("&") + item->text(1));
        }

        return;
    }
}

void SeerStackArgumentsBrowserWidget::handleItemExpanded (QTreeWidgetItem* item) {

    Q_UNUSED(item);

    argumentsTreeWidget->resizeColumnToContents(0);
    argumentsTreeWidget->resizeColumnToContents(1);
    argumentsTreeWidget->resizeColumnToContents(2);
    argumentsTreeWidget->resizeColumnToContents(3);
}

void SeerStackArgumentsBrowserWidget::handleItemCollapsed (QTreeWidgetItem* item) {

    Q_UNUSED(item);

    argumentsTreeWidget->resizeColumnToContents(0);
    argumentsTreeWidget->resizeColumnToContents(1);
    argumentsTreeWidget->resizeColumnToContents(2);
    argumentsTreeWidget->resizeColumnToContents(3);
}

void SeerStackArgumentsBrowserWidget::handleItemEntered (QTreeWidgetItem* item, int column) {

    Q_UNUSED(column);

    if (item->text(0) != "") {
        for (int i=0; i<argumentsTreeWidget->columnCount(); i++) { // The "level" item does not have a tooltip.
            item->setToolTip(i, "");
        }

    }else{
        QTreeWidgetItem* parent = item->parent(); // Get parent item, which is the level.

        item->setToolTip(0, parent->text(0) + " : " + item->text(1) + " : " + Seer::elideText(item->text(2), Qt::ElideRight, 100));

        for (int i=1; i<argumentsTreeWidget->columnCount(); i++) { // Copy tooltip to the other columns.
            item->setToolTip(i, item->toolTip(0));
        }
    }
}

void SeerStackArgumentsBrowserWidget::handleItemCreate (QTreeWidgetItem* parentItem, const QString& level_text, const QString& name_text, const QString& value_text) {

    // Instead of creating a new tree each time, we will reuse existing items, if they are there.
    // This allows the expanded items to remain expanded. We start by looking for matches that
    // may already be there. If there are matches, the code will reuse it.  If not, a new item
    // is created by the code. Note, when searching, we only look at the current level. Not any
    // children.
    QList<QTreeWidgetItem*> matches;

    if (parentItem == 0) {
        Q_ASSERT(parentItem != NULL);
        return;
    }else{
        for (int i=0; i<parentItem->childCount(); i++) {
            if (parentItem->child(i)->text(1) == name_text) {
                matches.append(parentItem->child(i));
            }
        }
    }

    // Parse bookmarks.
    QString capture0; // With const address.
    QString capture1; // Without.

    QRegularExpression withaddress_re("^@0[xX][0-9a-fA-F]+: \\{(.*?)\\}$");
    QRegularExpressionMatch withaddress_match = withaddress_re.match(value_text, 0, QRegularExpression::PartialPreferCompleteMatch);

    if (withaddress_match.hasMatch()) {
        capture0 = withaddress_match.captured(0);
        capture1 = withaddress_match.captured(1);

    }else{
        QRegularExpression noaddress_re("^\\{(.*?)\\}$");
        QRegularExpressionMatch noaddress_match   = noaddress_re.match(value_text, 0, QRegularExpression::PartialPreferCompleteMatch);

        if (noaddress_match.hasMatch()) {
            capture0 = noaddress_match.captured(0);
            capture1 = noaddress_match.captured(1);
        }
    }

    // Add the complex entry to the tree. Reuse, if possible.
    if (capture0 != "" && capture1 != "") {

        // Remove bookends
        QString text = capture1;

        QTreeWidgetItem* item = 0;

        // Use the privously created item. Or create a new one.
        if (matches.size() > 0) {
            item = matches[0];
            item->setText(3, "reused");

        }else{
            item = new QTreeWidgetItem;
            item->setText(3, "new");

            // If we're dealing with a top-level item, attach it to the tree.
            // Otherwise, attach it to the parent.
            if (parentItem) {
                parentItem->addChild(item);
            }else{
                argumentsTreeWidget->addTopLevelItem(item);
            }
        }

        // Set the flatvalue text.
        item->setText(0, level_text);
        item->setText(1, name_text);
        item->setText(2, Seer::filterEscapes(text));
        item->setFont(2, QFontDatabase::systemFont(QFontDatabase::FixedFont));

        // Convert to a list of name/value pairs.
        QStringList nv_pairs = Seer::parseCommaList(text, '{', '}');

        // Go through each pair and add the name and its value to the tree.
        for (const auto& nv : nv_pairs) {

            QStringPair pair = Seer::parseNameValue(nv, '=');

            handleItemCreate(item, level_text, pair.first, pair.second);
        }

    // Add the simple entry to the tree. Reuse, if possible.
    }else{
        QTreeWidgetItem* item = 0;

        // Use the privously created item. Or create a new one.
        if (matches.size() > 0) {
            item = matches[0];
            item->setText(3, "reused");

        }else{
            item = new QTreeWidgetItem;
            item->setText(3, "new");

            // If we're dealing with a top-level item, attach it to the tree.
            // Otherwise, attach it to the parent.
            if (parentItem) {
                parentItem->addChild(item);
            }else{
                argumentsTreeWidget->addTopLevelItem(item);
            }
        }

        // Simple entries don't have children. Delete them.
        QList<QTreeWidgetItem*> children = item->takeChildren();
        if (matches.size() > 0) {
            qDeleteAll(children);
        }

        // Populate the item.
        item->setText(0, level_text);
        item->setText(1, name_text);
        item->setText(2, Seer::filterEscapes(value_text));
        item->setFont(2, QFontDatabase::systemFont(QFontDatabase::FixedFont));
    }
}

void SeerStackArgumentsBrowserWidget::showEvent (QShowEvent* event) {

    QWidget::showEvent(event);

    refresh();
}

