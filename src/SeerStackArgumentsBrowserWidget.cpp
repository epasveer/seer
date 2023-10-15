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

    argumentsTreeWidget->clear();

    // Connect things.
    QObject::connect(argumentsTreeWidget, &QTreeWidget::customContextMenuRequested,    this,  &SeerStackArgumentsBrowserWidget::handleContextMenu);
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

        argumentsTreeWidget->clear();

        //qDebug() << text;

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

        //qDebug() << frame_list.count() << frame_list;

        for ( const auto& frame_text : frame_list  ) {

            //qDebug() << frame_text;

            QString level_text = Seer::parseFirst(frame_text, "level=", '"', '"', false);
            QString args_text  = Seer::parseFirst(frame_text, "args=",  '[', ']', false);

            //qDebug() << level_text;
            //qDebug() << args_text;

            QStringList namevalue_list  = Seer::parse(args_text, "",  '{', '}', false);

            // Add the level to the tree.
            QTreeWidgetItem* topItem = new QTreeWidgetItem;
            topItem->setText(0, level_text);

            argumentsTreeWidget->addTopLevelItem(topItem);

            // Get the argument names and values for the level.
            for ( const auto& namevalue_text : namevalue_list  ) {

                QString name_text  = Seer::parseFirst(namevalue_text, "name=",  '"', '"', false);
                QString value_text = Seer::parseFirst(namevalue_text, "value=", '"', '"', false);

                QTreeWidgetItem* item = new QTreeWidgetItem;
                item->setText(1, name_text); // Set the name and value. Don't set the level.
                item->setText(2, Seer::filterEscapes(value_text));
                item->setFont(2, QFontDatabase::systemFont(QFontDatabase::FixedFont));

                topItem->addChild(item);
            }

            // Expand all items for the level.
            argumentsTreeWidget->expandItem(topItem);
        }

    }else if (text.startsWith("^error,msg=\"No registers.\"")) {
        argumentsTreeWidget->clear();

    }else{
        // Ignore others.
    }

    argumentsTreeWidget->resizeColumnToContents(0);
    argumentsTreeWidget->resizeColumnToContents(1);
    argumentsTreeWidget->resizeColumnToContents(2);

    QApplication::restoreOverrideCursor();
}

void SeerStackArgumentsBrowserWidget::handleStoppingPointReached () {

    // Don't do any work if the widget is hidden.
    if (isHidden()) {
        return;
    }

    refresh();
}

void SeerStackArgumentsBrowserWidget::refresh () {
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

    addVariableLoggerExpressionAction                   = new QAction(QString("\"%1\"").arg(item->text(1)));
    addVariableLoggerAsteriskExpressionAction           = new QAction(QString("\"*%1\"").arg(item->text(1)));
    addVariableLoggerAmpersandExpressionAction          = new QAction(QString("\"&&%1\"").arg(item->text(1)));
    addVariableLoggerAsteriskAmpersandExpressionAction  = new QAction(QString("\"*&&%1\"").arg(item->text(1)));
    addVariableTrackerExpressionAction                  = new QAction(QString("\"%1\"").arg(item->text(1)));
    addVariableTrackerAsteriskExpressionAction          = new QAction(QString("\"*%1\"").arg(item->text(1)));
    addVariableTrackerAmpersandExpressionAction         = new QAction(QString("\"&&%1\"").arg(item->text(1)));
    addVariableTrackerAsteriskAmpersandExpressionAction = new QAction(QString("\"*&&%1\"").arg(item->text(1)));
    addMemoryVisualizerAction                           = new QAction(QString("\"%1\"").arg(item->text(1)));
    addMemoryAsteriskVisualizerAction                   = new QAction(QString("\"*%1\"").arg(item->text(1)));
    addMemoryAmpersandVisualizerAction                  = new QAction(QString("\"&&%1\"").arg(item->text(1)));
    addArrayVisualizerAction                            = new QAction(QString("\"%1\"").arg(item->text(1)));
    addArrayAsteriskVisualizerAction                    = new QAction(QString("\"*%1\"").arg(item->text(1)));
    addArrayAmpersandVisualizerAction                   = new QAction(QString("\"&&%1\"").arg(item->text(1)));
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
    QAction* action = menu.exec(argumentsTreeWidget->viewport()->mapToGlobal(pos));

    // Do nothing.
    if (action == 0) {
        return;
    }

    // Handle adding a variable to log.
    if (action == addVariableLoggerExpressionAction) {

        //qDebug() << "addVariableLoggerExpression" << item->text(1);

        // Emit the signals.
        if (item->text(1) != "") {
            emit addVariableLoggerExpression(item->text(1));
        }

        return;
    }

    // Handle adding a variable to log.
    if (action == addVariableLoggerAsteriskExpressionAction) {

        //qDebug() << "addVariableLoggerAsteriskExpression" << item->text(1);

        // Emit the signals.
        if (item->text(1) != "") {
            emit addVariableLoggerExpression(QString("*") + item->text(1));
        }

        return;
    }

    // Handle adding a variable to log.
    if (action == addVariableLoggerAmpersandExpressionAction) {

        //qDebug() << "addVariableLoggerAmpersandExpression" << item->text(1);

        // Emit the signals.
        if (item->text(1) != "") {
            emit addVariableLoggerExpression(QString("&") + item->text(1));
        }

        return;
    }

    // Handle adding a variable to log.
    if (action == addVariableLoggerAsteriskAmpersandExpressionAction) {

        //qDebug() << "addVariableLoggerAsteriskAmpersandExpression" << item->text(1);

        // Emit the signals.
        if (item->text(1) != "") {
            emit addVariableLoggerExpression(QString("*&") + item->text(1));
        }

        return;
    }

    // Handle adding a variable to track.
    if (action == addVariableTrackerExpressionAction) {

        //qDebug() << "addVariableTrackerExpression" << item->text(1);

        // Emit the signals.
        if (item->text(1) != "") {
            emit addVariableTrackerExpression(item->text(1));
            emit refreshVariableTrackerValues();
        }

        return;
    }

    // Handle adding a variable to track.
    if (action == addVariableTrackerAsteriskExpressionAction) {

        //qDebug() << "addVariableTrackerAsteriskExpression" << item->text(1);

        // Emit the signals.
        if (item->text(1) != "") {
            emit addVariableTrackerExpression(QString("*") + item->text(1));
            emit refreshVariableTrackerValues();
        }

        return;
    }

    // Handle adding a variable to track.
    if (action == addVariableTrackerAmpersandExpressionAction) {

        //qDebug() << "addVariableTrackerAmpersandExpression" << item->text(1);

        // Emit the signals.
        if (item->text(1) != "") {
            emit addVariableTrackerExpression(QString("&") + item->text(1));
            emit refreshVariableTrackerValues();
        }

        return;
    }

    // Handle adding a variable to track.
    if (action == addVariableTrackerAsteriskAmpersandExpressionAction) {

        //qDebug() << "addVariableTrackerAsteriskAmpersandExpression" << item->text(1);

        // Emit the signals.
        if (item->text(1) != "") {
            emit addVariableTrackerExpression(QString("*&") + item->text(1));
            emit refreshVariableTrackerValues();
        }

        return;
    }

    // Handle adding memory to visualize.
    if (action == addMemoryVisualizerAction) {

        //qDebug() << "addMemoryVisualizer" << item->text(1);

        // Emit the signals.
        if (item->text(1) != "") {
            emit addMemoryVisualize(item->text(1));
        }

        return;
    }

    // Handle adding memory to visualize.
    if (action == addMemoryAsteriskVisualizerAction) {

        //qDebug() << "addMemoryAsteriskVisualizer" << item->text(1);

        // Emit the signals.
        if (item->text(1) != "") {
            emit addMemoryVisualize(QString("*") + item->text(1));
        }

        return;
    }

    // Handle adding memory to visualize.
    if (action == addMemoryAmpersandVisualizerAction) {

        //qDebug() << "addMemoryAmpersandVisualizer" << item->text(1);

        // Emit the signals.
        if (item->text(1) != "") {
            emit addMemoryVisualize(QString("&") + item->text(1));
        }

        return;
    }

    // Handle adding array to visualize.
    if (action == addArrayVisualizerAction) {

        //qDebug() << "addArrayVisualizer" << item->text(1);

        // Emit the signals.
        if (item->text(1) != "") {
            emit addArrayVisualize(item->text(1));
        }

        return;
    }

    // Handle adding array to visualize.
    if (action == addArrayAsteriskVisualizerAction) {

        //qDebug() << "addArrayAsteriskVisualizer" << item->text(1);

        // Emit the signals.
        if (item->text(1) != "") {
            emit addArrayVisualize(QString("*") + item->text(1));
        }

        return;
    }

    // Handle adding array to visualize.
    if (action == addArrayAmpersandVisualizerAction) {

        //qDebug() << "addArrayAmpersandVisualizer" << item->text(1);

        // Emit the signals.
        if (item->text(1) != "") {
            emit addArrayVisualize(QString("&") + item->text(1));
        }

        return;
    }

    // Handle adding struct to visualize.
    if (action == addStructVisualizerAction) {

        //qDebug() << "addStructVisualizer" << item->text(1);

        // Emit the signals.
        if (item->text(1) != "") {
            emit addStructVisualize(item->text(1));
        }

        return;
    }

    // Handle adding array to visualize.
    if (action == addStructAsteriskVisualizerAction) {

        //qDebug() << "addStructAsteriskVisualizer" << item->text(1);

        // Emit the signals.
        if (item->text(1) != "") {
            emit addStructVisualize(QString("*") + item->text(1));
        }

        return;
    }

    // Handle adding array to visualize.
    if (action == addStructAmpersandVisualizerAction) {

        //qDebug() << "addStructAmpersandVisualizer" << item->text(1);

        // Emit the signals.
        if (item->text(1) != "") {
            emit addStructVisualize(QString("&") + item->text(1));
        }

        return;
    }
}

void SeerStackArgumentsBrowserWidget::handleItemEntered (QTreeWidgetItem* item, int column) {

    Q_UNUSED(column);

    //qDebug() << item->text(0) << column;

    if (item->text(0) != "") {
        for (int i=0; i<argumentsTreeWidget->columnCount(); i++) { // The "level" item does not have a tooltip.
            item->setToolTip(i, "");
        }

    }else{
        QTreeWidgetItem* parent = item->parent(); // Get parent item, which is the level.

        item->setToolTip(0, parent->text(0) + " : " + item->text(1) + " : " + item->text(2));

        for (int i=1; i<argumentsTreeWidget->columnCount(); i++) { // Copy tooltip to the other columns.
            item->setToolTip(i, item->toolTip(0));
        }
    }
}

void SeerStackArgumentsBrowserWidget::showEvent (QShowEvent* event) {

    QWidget::showEvent(event);

    refresh();
}

