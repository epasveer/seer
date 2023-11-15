#include "SeerStackLocalsBrowserWidget.h"
#include "SeerUtl.h"
#include <QtWidgets/QTreeWidget>
#include <QtWidgets/QTreeWidgetItemIterator>
#include <QtWidgets/QApplication>
#include <QtWidgets/QMenu>
#include <QtGui/QFontDatabase>
#include <QtCore/QRegularExpressionMatch>
#include <QAction>
#include <QtCore/QDebug>
#include <QtGlobal>

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
    localsTreeWidget->setColumnHidden(3, true);  // Hide the 'used' column.
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

        // ^done,variables=[
        //     {name=\"message\",arg=\"1\",value=\"\\\"Hello, World!\\\"\"},
        //     {name=\"something\",arg=\"1\",value=\"\\\"Hello, World!\\\"\"}
        // ]

        // Parse the text. Create a list of variables.
        QString frame_text = Seer::parseFirst(text, "variables=", '[', ']', false);

        QStringList variable_list = Seer::parse(frame_text, "", '{', '}', false);

        // Mark each entry initially as "unused".
        // Later, some will be marked as "reused" or "new". Then the "unused" ones will
        // be deleted.
        QTreeWidgetItemIterator it(localsTreeWidget);
        while (*it) {
            (*it)->setText(3, "unused");
            ++it;
        }

        // Loop through each variable.
        for (const auto& variable_text : variable_list) {

            QString name_text  = Seer::parseFirst(variable_text, "name=",  '"', '"', false);
            QString arg_text   = Seer::parseFirst(variable_text, "arg=",   '"', '"', false);
            QString value_text = Seer::parseFirst(variable_text, "value=", '"', '"', false);

            // Morph 'is function argument' from a '1' to 'yes'.
            arg_text = (arg_text == "1" ? "yes" : "");

            // Populate the tree.
            handleItemCreate(0, name_text, arg_text, value_text);
        }

        // At this point, there are some new entries, some reused entries, and some unused ones.
        // Delete the unused ones. They are obsolete.
        // Don't use qDeleteAll() here. It doesn't work as expected for items that are "found".
        // Instead, get a list of matches and delete them from the bottom up.
        QList<QTreeWidgetItem*> matches = localsTreeWidget->findItems("unused", Qt::MatchExactly|Qt::MatchRecursive, 3);

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

    // Build up a variable string, incase it is a nested struct.
    QTreeWidgetItem* i = item;
    QString variable = item->text(0);
    while (i->parent() != 0) {
        variable = i->parent()->text(0) + "." + variable;
        i = i->parent();
    }

    // Handle adding a variable to log.
    if (action == addVariableLoggerExpressionAction) {

        //qDebug() << "addVariableLoggerExpression" << variable;

        // Emit the signals.
        if (variable != "") {
            emit addVariableLoggerExpression(variable);
        }

        return;
    }

    // Handle adding a variable to log.
    if (action == addVariableLoggerAsteriskExpressionAction) {

        //qDebug() << "addVariableLoggerAsteriskExpression" << variable;

        // Emit the signals.
        if (variable != "") {
            emit addVariableLoggerExpression(QString("*") + variable);
        }

        return;
    }

    // Handle adding a variable to log.
    if (action == addVariableLoggerAmpersandExpressionAction) {

        //qDebug() << "addVariableLoggerAmpersandExpression" << variable;

        // Emit the signals.
        if (variable != "") {
            emit addVariableLoggerExpression(QString("&") + variable);
        }

        return;
    }

    // Handle adding a variable to log.
    if (action == addVariableLoggerAsteriskAmpersandExpressionAction) {

        //qDebug() << "addVariableLoggerAsteriskAmpersandExpression" << variable;

        // Emit the signals.
        if (variable != "") {
            emit addVariableLoggerExpression(QString("*&") + variable);
        }

        return;
    }

    // Handle adding a variable to track.
    if (action == addVariableTrackerExpressionAction) {

        //qDebug() << "addVariableTrackerExpression" << variable;

        // Emit the signals.
        if (variable != "") {
            emit addVariableTrackerExpression(variable);
            emit refreshVariableTrackerValues();
        }

        return;
    }

    // Handle adding a variable to track.
    if (action == addVariableTrackerAsteriskExpressionAction) {

        //qDebug() << "addVariableTrackerAsteriskExpression" << variable;

        // Emit the signals.
        if (variable != "") {
            emit addVariableTrackerExpression(QString("*") + variable);
            emit refreshVariableTrackerValues();
        }

        return;
    }

    // Handle adding a variable to track.
    if (action == addVariableTrackerAmpersandExpressionAction) {

        //qDebug() << "addVariableTrackerAmpersandExpression" << variable;

        // Emit the signals.
        if (variable != "") {
            emit addVariableTrackerExpression(QString("&") + variable);
            emit refreshVariableTrackerValues();
        }

        return;
    }

    // Handle adding a variable to track.
    if (action == addVariableTrackerAsteriskAmpersandExpressionAction) {

        //qDebug() << "addVariableTrackerAsteriskAmpersandExpression" << variable;

        // Emit the signals.
        if (variable != "") {
            emit addVariableTrackerExpression(QString("*&") + variable);
            emit refreshVariableTrackerValues();
        }

        return;
    }

    // Handle adding memory to visualize.
    if (action == addMemoryVisualizerAction) {

        //qDebug() << "addMemoryVisualizer" << variable;

        // Emit the signals.
        if (variable != "") {
            emit addMemoryVisualize(variable);
        }

        return;
    }

    // Handle adding memory to visualize.
    if (action == addMemoryAsteriskVisualizerAction) {

        //qDebug() << "addMemoryAsteriskVisualizer" << variable;

        // Emit the signals.
        if (variable != "") {
            emit addMemoryVisualize(QString("*") + variable);
        }

        return;
    }

    // Handle adding memory to visualize.
    if (action == addMemoryAmpersandVisualizerAction) {

        //qDebug() << "addMemoryAmpersandVisualizer" << variable;

        // Emit the signals.
        if (variable != "") {
            emit addMemoryVisualize(QString("&") + variable);
        }

        return;
    }

    // Handle adding array to visualize.
    if (action == addArrayVisualizerAction) {

        //qDebug() << "addArrayVisualizer" << variable;

        // Emit the signals.
        if (variable != "") {
            emit addArrayVisualize(variable);
        }

        return;
    }

    // Handle adding array to visualize.
    if (action == addArrayAsteriskVisualizerAction) {

        //qDebug() << "addArrayAsteriskVisualizer" << variable;

        // Emit the signals.
        if (variable != "") {
            emit addArrayVisualize(QString("*") + variable);
        }

        return;
    }

    // Handle adding array to visualize.
    if (action == addArrayAmpersandVisualizerAction) {

        //qDebug() << "addArrayAmpersandVisualizer" << variable;

        // Emit the signals.
        if (variable != "") {
            emit addArrayVisualize(QString("&") + variable);
        }

        return;
    }

    // Handle adding struct to visualize.
    if (action == addStructVisualizerAction) {

        //qDebug() << "addStructVisualizer" << variable;

        // Emit the signals.
        if (variable != "") {
            emit addStructVisualize(variable);
        }

        return;
    }

    // Handle adding struct to visualize.
    if (action == addStructAsteriskVisualizerAction) {

        //qDebug() << "addStructAsteriskVisualizer" << variable;

        // Emit the signals.
        if (variable != "") {
            emit addStructVisualize(QString("*") + variable);
        }

        return;
    }

    // Handle adding struct to visualize.
    if (action == addStructAmpersandVisualizerAction) {

        //qDebug() << "addStructAmpersandVisualizer" << variable;

        // Emit the signals.
        if (variable != "") {
            emit addStructVisualize(QString("&") + variable);
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

void SeerStackLocalsBrowserWidget::handleItemCreate (QTreeWidgetItem* parentItem, const QString& name_text, const QString& arg_text, const QString& value_text) {

    // Instead of creating a new tree each time, we will reuse existing items, if they are there.
    // This allows the expanded items to remain expanded. We start by looking for matches that
    // may already be there. If there are matches, the code will reuse it.  If not, a new item
    // is created by the code. Note, when searching, we only look at the current level. Not any
    // children.
    QList<QTreeWidgetItem*> matches;

    if (parentItem == 0) {
        matches = localsTreeWidget->findItems(name_text, Qt::MatchExactly, 0);
    }else{
        for (int i=0; i<parentItem->childCount(); i++) {
            if (parentItem->child(i)->text(0) == name_text) {
                matches.append(parentItem->child(i));
            }
        }
    }

    // Parse bookmarks.
    QString capture0; // With bookends.
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
                localsTreeWidget->addTopLevelItem(item);
            }
        }

        // Set the flatvalue text.
        item->setText(0, name_text);
        item->setText(1, arg_text);
        item->setText(2, Seer::filterEscapes(text));
        item->setFont(2, QFontDatabase::systemFont(QFontDatabase::FixedFont));

        // Convert to a list of name/value pairs.
        QStringList nv_pairs = Seer::parseCommaList(text, '{', '}');

        // Go through each pair and add the name and its value to the tree.
        for (const auto& nv : nv_pairs) {

            QStringPair pair = Seer::parseNameValue(nv, '=');

            handleItemCreate(item, pair.first, arg_text, pair.second);
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
                localsTreeWidget->addTopLevelItem(item);
            }
        }

        // Simple entries don't have children. Delete them.
        QList<QTreeWidgetItem*> children = item->takeChildren();
        if (matches.size() > 0) {
            qDeleteAll(children);
        }

        // Populate the item.
        item->setText(0, name_text);
        item->setText(1, arg_text);
        item->setText(2, Seer::filterEscapes(value_text));
        item->setFont(2, QFontDatabase::systemFont(QFontDatabase::FixedFont));
    }
}

void SeerStackLocalsBrowserWidget::showEvent (QShowEvent* event) {

    QWidget::showEvent(event);

    refresh();
}

