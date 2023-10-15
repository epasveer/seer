#include "SeerStackLocalsBrowserWidget.h"
#include "SeerUtl.h"
#include <QtWidgets/QTreeWidget>
#include <QtWidgets/QTreeWidgetItemIterator>
#include <QtWidgets/QApplication>
#include <QtWidgets/QMenu>
#include <QtGui/QFontDatabase>
#include <QAction>
#include <QtCore/QDebug>

SeerStackLocalsBrowserWidget::SeerStackLocalsBrowserWidget (QWidget* parent) : QWidget(parent) {

    // Construct the UI.
    setupUi(this);

    // Setup the widgets
    localsTreeWidget->setContextMenuPolicy(Qt::CustomContextMenu);
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
    QObject::connect(localsTreeWidget, &QTreeWidget::customContextMenuRequested,    this,  &SeerStackLocalsBrowserWidget::handleContextMenu);
    QObject::connect(localsTreeWidget, &QTreeWidget::itemCollapsed,                 this,  &SeerStackLocalsBrowserWidget::handleItemCollapsed);
    QObject::connect(localsTreeWidget, &QTreeWidget::itemExpanded,                  this,  &SeerStackLocalsBrowserWidget::handleItemExpanded);
    QObject::connect(localsTreeWidget, &QTreeWidget::itemEntered,                   this,  &SeerStackLocalsBrowserWidget::handleItemEntered);
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
                    item->setFont(2, QFontDatabase::systemFont(QFontDatabase::FixedFont));
                    item->setText(3, "new");

                    topItem->addChild(item);

                // Add the simple entry to the tree.
                }else{
                    QTreeWidgetItem* item = new QTreeWidgetItem;
                    item->setText(0, name_text);
                    item->setText(1, arg_text);
                    item->setText(2, Seer::filterEscapes(value_text));
                    item->setFont(2, QFontDatabase::systemFont(QFontDatabase::FixedFont));
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
                    child->setFont(2, QFontDatabase::systemFont(QFontDatabase::FixedFont));
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

void SeerStackLocalsBrowserWidget::handleContextMenu (const QPoint& pos) {

    QTreeWidgetItem* item = localsTreeWidget->itemAt(pos);

    if (item == 0) {
        return;
    }

    QAction* addVariableLoggerExpressionAction;
    QAction* addVariableLoggerAsteriskExpressionAction;
    QAction* addVariableLoggerAmpersandExpressionAction;
    QAction* addVariableLoggerAsteriskAmpersandExpressionAction;
    QAction* addVariableTrackerExpressionAction;
    QAction* addVariableTrackerAsteriskExpressionAction;
    QAction* addVariableTrackerAmpersandExpressionAction;
    QAction* addVariableTrackerAsteriskAmpersandExpressionAction;
    QAction* addMemoryVisualizerAction;
    QAction* addMemoryAsteriskVisualizerAction;
    QAction* addMemoryAmpersandVisualizerAction;
    QAction* addArrayVisualizerAction;
    QAction* addArrayAsteriskVisualizerAction;
    QAction* addArrayAmpersandVisualizerAction;
    QAction* addStructVisualizerAction;
    QAction* addStructAsteriskVisualizerAction;
    QAction* addStructAmpersandVisualizerAction;

    addVariableLoggerExpressionAction                   = new QAction(QString("\"%1\"").arg(item->text(0)));
    addVariableLoggerAsteriskExpressionAction           = new QAction(QString("\"*%1\"").arg(item->text(0)));
    addVariableLoggerAmpersandExpressionAction          = new QAction(QString("\"&&%1\"").arg(item->text(0)));
    addVariableLoggerAsteriskAmpersandExpressionAction  = new QAction(QString("\"*&&%1\"").arg(item->text(0)));
    addVariableTrackerExpressionAction                  = new QAction(QString("\"%1\"").arg(item->text(0)));
    addVariableTrackerAsteriskExpressionAction          = new QAction(QString("\"*%1\"").arg(item->text(0)));
    addVariableTrackerAmpersandExpressionAction         = new QAction(QString("\"&&%1\"").arg(item->text(0)));
    addVariableTrackerAsteriskAmpersandExpressionAction = new QAction(QString("\"*&&%1\"").arg(item->text(0)));
    addMemoryVisualizerAction                           = new QAction(QString("\"%1\"").arg(item->text(0)));
    addMemoryAsteriskVisualizerAction                   = new QAction(QString("\"*%1\"").arg(item->text(0)));
    addMemoryAmpersandVisualizerAction                  = new QAction(QString("\"&&%1\"").arg(item->text(0)));
    addArrayVisualizerAction                            = new QAction(QString("\"%1\"").arg(item->text(0)));
    addArrayAsteriskVisualizerAction                    = new QAction(QString("\"*%1\"").arg(item->text(0)));
    addArrayAmpersandVisualizerAction                   = new QAction(QString("\"&&%1\"").arg(item->text(0)));
    addStructVisualizerAction                           = new QAction(QString("\"%1\"").arg(item->text(0)));
    addStructAsteriskVisualizerAction                   = new QAction(QString("\"*%1\"").arg(item->text(0)));
    addStructAmpersandVisualizerAction                  = new QAction(QString("\"&&%1\"").arg(item->text(0)));

    QMenu menu("Visualizers", this);
    menu.setTitle("Visualizers");

    QMenu loggerMenu("Add variable to Logger");
    loggerMenu.addAction(addVariableLoggerExpressionAction);
    loggerMenu.addAction(addVariableLoggerAsteriskExpressionAction);
    loggerMenu.addAction(addVariableLoggerAmpersandExpressionAction);
    loggerMenu.addAction(addVariableLoggerAsteriskAmpersandExpressionAction);
    menu.addMenu(&loggerMenu);

    QMenu trackerMenu("Add variable to Tracker");
    trackerMenu.addAction(addVariableTrackerExpressionAction);
    trackerMenu.addAction(addVariableTrackerAsteriskExpressionAction);
    trackerMenu.addAction(addVariableTrackerAmpersandExpressionAction);
    trackerMenu.addAction(addVariableTrackerAsteriskAmpersandExpressionAction);
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

    QMenu structVisualizerMenu("Add variable to a Struct Visualizer");
    structVisualizerMenu.addAction(addStructVisualizerAction);
    structVisualizerMenu.addAction(addStructAsteriskVisualizerAction);
    structVisualizerMenu.addAction(addStructAmpersandVisualizerAction);
    menu.addMenu(&structVisualizerMenu);

    // Launch the menu. Get the response.
    QAction* action = menu.exec(localsTreeWidget->viewport()->mapToGlobal(pos));

    // Do nothing.
    if (action == 0) {
        return;
    }

    // Handle adding a variable to log.
    if (action == addVariableLoggerExpressionAction) {

        //qDebug() << "addVariableLoggerExpression" << item->text(0);

        // Emit the signals.
        if (item->text(0) != "") {
            emit addVariableLoggerExpression(item->text(0));
        }

        return;
    }

    // Handle adding a variable to log.
    if (action == addVariableLoggerAsteriskExpressionAction) {

        //qDebug() << "addVariableLoggerAsteriskExpression" << item->text(0);

        // Emit the signals.
        if (item->text(0) != "") {
            emit addVariableLoggerExpression(QString("*") + item->text(0));
        }

        return;
    }

    // Handle adding a variable to log.
    if (action == addVariableLoggerAmpersandExpressionAction) {

        //qDebug() << "addVariableLoggerAmpersandExpression" << item->text(0);

        // Emit the signals.
        if (item->text(0) != "") {
            emit addVariableLoggerExpression(QString("&") + item->text(0));
        }

        return;
    }

    // Handle adding a variable to log.
    if (action == addVariableLoggerAsteriskAmpersandExpressionAction) {

        //qDebug() << "addVariableLoggerAsteriskAmpersandExpression" << item->text(0);

        // Emit the signals.
        if (item->text(0) != "") {
            emit addVariableLoggerExpression(QString("*&") + item->text(0));
        }

        return;
    }

    // Handle adding a variable to track.
    if (action == addVariableTrackerExpressionAction) {

        //qDebug() << "addVariableTrackerExpression" << item->text(0);

        // Emit the signals.
        if (item->text(0) != "") {
            emit addVariableTrackerExpression(item->text(0));
            emit refreshVariableTrackerValues();
        }

        return;
    }

    // Handle adding a variable to track.
    if (action == addVariableTrackerAsteriskExpressionAction) {

        //qDebug() << "addVariableTrackerAsteriskExpression" << item->text(0);

        // Emit the signals.
        if (item->text(0) != "") {
            emit addVariableTrackerExpression(QString("*") + item->text(0));
            emit refreshVariableTrackerValues();
        }

        return;
    }

    // Handle adding a variable to track.
    if (action == addVariableTrackerAmpersandExpressionAction) {

        //qDebug() << "addVariableTrackerAmpersandExpression" << item->text(0);

        // Emit the signals.
        if (item->text(0) != "") {
            emit addVariableTrackerExpression(QString("&") + item->text(0));
            emit refreshVariableTrackerValues();
        }

        return;
    }

    // Handle adding a variable to track.
    if (action == addVariableTrackerAsteriskAmpersandExpressionAction) {

        //qDebug() << "addVariableTrackerAsteriskAmpersandExpression" << item->text(0);

        // Emit the signals.
        if (item->text(0) != "") {
            emit addVariableTrackerExpression(QString("*&") + item->text(0));
            emit refreshVariableTrackerValues();
        }

        return;
    }

    // Handle adding memory to visualize.
    if (action == addMemoryVisualizerAction) {

        //qDebug() << "addMemoryVisualizer" << item->text(0);

        // Emit the signals.
        if (item->text(0) != "") {
            emit addMemoryVisualize(item->text(0));
        }

        return;
    }

    // Handle adding memory to visualize.
    if (action == addMemoryAsteriskVisualizerAction) {

        //qDebug() << "addMemoryAsteriskVisualizer" << item->text(0);

        // Emit the signals.
        if (item->text(0) != "") {
            emit addMemoryVisualize(QString("*") + item->text(0));
        }

        return;
    }

    // Handle adding memory to visualize.
    if (action == addMemoryAmpersandVisualizerAction) {

        //qDebug() << "addMemoryAmpersandVisualizer" << item->text(0);

        // Emit the signals.
        if (item->text(0) != "") {
            emit addMemoryVisualize(QString("&") + item->text(0));
        }

        return;
    }

    // Handle adding array to visualize.
    if (action == addArrayVisualizerAction) {

        //qDebug() << "addArrayVisualizer" << item->text(0);

        // Emit the signals.
        if (item->text(0) != "") {
            emit addArrayVisualize(item->text(0));
        }

        return;
    }

    // Handle adding array to visualize.
    if (action == addArrayAsteriskVisualizerAction) {

        //qDebug() << "addArrayAsteriskVisualizer" << item->text(0);

        // Emit the signals.
        if (item->text(0) != "") {
            emit addArrayVisualize(QString("*") + item->text(0));
        }

        return;
    }

    // Handle adding array to visualize.
    if (action == addArrayAmpersandVisualizerAction) {

        //qDebug() << "addArrayAmpersandVisualizer" << item->text(0);

        // Emit the signals.
        if (item->text(0) != "") {
            emit addArrayVisualize(QString("&") + item->text(0));
        }

        return;
    }

    // Handle adding struct to visualize.
    if (action == addStructVisualizerAction) {

        //qDebug() << "addStructVisualizer" << item->text(0);

        // Emit the signals.
        if (item->text(0) != "") {
            emit addStructVisualize(item->text(0));
        }

        return;
    }

    // Handle adding struct to visualize.
    if (action == addStructAsteriskVisualizerAction) {

        //qDebug() << "addStructAsteriskVisualizer" << item->text(0);

        // Emit the signals.
        if (item->text(0) != "") {
            emit addStructVisualize(QString("*") + item->text(0));
        }

        return;
    }

    // Handle adding struct to visualize.
    if (action == addStructAmpersandVisualizerAction) {

        //qDebug() << "addStructAmpersandVisualizer" << item->text(0);

        // Emit the signals.
        if (item->text(0) != "") {
            emit addStructVisualize(QString("&") + item->text(0));
        }

        return;
    }
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

