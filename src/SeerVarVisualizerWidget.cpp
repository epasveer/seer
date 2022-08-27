#include "SeerVarVisualizerWidget.h"
#include "SeerUtl.h"
#include <QtWidgets/QTreeWidget>
#include <QtWidgets/QTreeWidgetItemIterator>
#include <QtWidgets/QMenu>
#include <QtWidgets/QAction>
#include <QtGui/QIcon>
#include <QtCore/QRegExp>
#include <QtCore/QTimer>
#include <QtCore/QSettings>
#include <QtCore/QDebug>

SeerVarVisualizerWidget::SeerVarVisualizerWidget (QWidget* parent) : QWidget(parent) {

    // Init variables.
    _variableId = Seer::createID(); // Create id for queries.

    // Set up UI.
    setupUi(this);

    // Setup the widgets
    setWindowIcon(QIcon(":/seer/resources/seergdb_64x64.png"));
    setWindowTitle("Seer Var Visualizer");
    setAttribute(Qt::WA_DeleteOnClose);

    variableNameLineEdit->setFocus();
    variableTreeWidget->setContextMenuPolicy(Qt::CustomContextMenu);
    variableTreeWidget->setMouseTracking(true);
    variableTreeWidget->setSortingEnabled(false);
    variableTreeWidget->setSelectionMode(QAbstractItemView::ExtendedSelection);
    variableTreeWidget->setRootIsDecorated(true);
    variableTreeWidget->setItemsExpandable(true);
    variableTreeWidget->resizeColumnToContents(0); // varobj name
    variableTreeWidget->resizeColumnToContents(1); // variable
    variableTreeWidget->resizeColumnToContents(2); // exp
    variableTreeWidget->resizeColumnToContents(3); // numchild
    variableTreeWidget->resizeColumnToContents(4); // value
    variableTreeWidget->resizeColumnToContents(5); // type
    variableTreeWidget->resizeColumnToContents(6); // thread-id
    variableTreeWidget->resizeColumnToContents(7); // has_more
    variableTreeWidget->clear();

    // Connect things.
    QObject::connect(expandAllToolButton,    &QToolButton::clicked,                       this,  &SeerVarVisualizerWidget::handleExpandAllButton);
    QObject::connect(collapseAllToolButton,  &QToolButton::clicked,                       this,  &SeerVarVisualizerWidget::handleCollapseAllButton);
    QObject::connect(refreshToolButton,      &QToolButton::clicked,                       this,  &SeerVarVisualizerWidget::handleRefreshButton);
    QObject::connect(variableNameLineEdit,   &QLineEdit::returnPressed,                   this,  &SeerVarVisualizerWidget::handleVariableNameLineEdit);
    QObject::connect(variableTreeWidget,     &QTreeWidget::customContextMenuRequested,    this,  &SeerVarVisualizerWidget::handleContextMenu);
    QObject::connect(variableTreeWidget,     &QTreeWidget::itemEntered,                   this,  &SeerVarVisualizerWidget::handleItemEntered);
    QObject::connect(variableTreeWidget,     &QTreeWidget::itemExpanded,                  this,  &SeerVarVisualizerWidget::handleItemExpanded);
    QObject::connect(variableTreeWidget,     &QTreeWidget::itemCollapsed,                 this,  &SeerVarVisualizerWidget::handleItemExpanded);

    // Restore window settings.
    readSettings();
}

SeerVarVisualizerWidget::~SeerVarVisualizerWidget () {

    // Send signal to delete variable.
    if (_variableName != "") {
        emit varObjDelete(_variableId, _variableName);
    }
}

void SeerVarVisualizerWidget::setVariableName (const QString& name) {

    setWindowTitle("Seer Var Visualizer - '" + name + "'");

    variableNameLineEdit->setText(name);

    // Send signal to delete old variable.
    if (_variableName != "") {

        emit varObjDelete(_variableId, _variableName);

        _variableName = "";
    }

    // Create the initial variable in the tree.
    variableTreeWidget->clear();

    if (variableNameLineEdit->text() != "") {
        QTreeWidgetItem* item = new QTreeWidgetItem;
        item->setText(0, "");
        item->setText(1, name);
        item->setText(2, "");
        item->setText(3, "");
        item->setText(4, "");
        item->setText(5, "");
        item->setText(6, "");

        variableTreeWidget->addTopLevelItem(item);
    }

    // Resize columns.
    handleResizeColumns();

    // Send signal to get variable result.
    if (variableNameLineEdit->text() != "") {
        emit varObjCreate(_variableId, variableNameLineEdit->text());
    }
}

QString SeerVarVisualizerWidget::variableName () const {
    return variableNameLineEdit->text();
}

void SeerVarVisualizerWidget::handleText (const QString& text) {

    QApplication::setOverrideCursor(Qt::BusyCursor);

    if (text.contains(QRegExp("^([0-9]+)\\^done,name="))) {

        // "4^done,name=\"seer4\",numchild=\"1\",value=\"{...}\",type=\"Person\",thread-id=\"1\",has_more=\"0\""

        // qDebug() << text;

        QString id_text = text.section('^', 0,0);

        if (id_text.toInt() == _variableId) {

            QString name_text     = Seer::parseFirst(text, "name=",       '"', '"', false);
            QString exp_text      = Seer::parseFirst(text, "exp=",        '"', '"', false);
            QString numchild_text = Seer::parseFirst(text, "numchild=",   '"', '"', false);
            QString value_text    = Seer::parseFirst(text, "value=",      '"', '"', false);
            QString type_text     = Seer::parseFirst(text, "type=",       '"', '"', false);
            QString threadid_text = Seer::parseFirst(text, "thread-id=",  '"', '"', false);
            QString hasmore_text  = Seer::parseFirst(text, "has_more=",   '"', '"', false);

            QTreeWidgetItem* topItem = variableTreeWidget->topLevelItem(0);
            if (topItem == 0) {
                return;
            }

            topItem->setText(0, name_text);
            topItem->setText(2, exp_text);
            topItem->setText(3, numchild_text);
            topItem->setText(4, Seer::filterEscapes(value_text));
            topItem->setText(5, type_text);
            topItem->setText(6, threadid_text);
            topItem->setText(7, hasmore_text);

            // For now, always expand everything.
            variableTreeWidget->expandAll();

            // If there are children, get them.
            if (numchild_text != "0") {
                emit varObjListChildren(_variableId, name_text);
            }

            // Save the VarObj name.
            _variableName = name_text;
        }


    }else if (text.contains(QRegExp("^([0-9]+)\\^done,numchild="))) {

        // "4^done,numchild="1", children=[
        //                                 child={name="x2112.public",exp="public",numchild="4",thread-id="1"} 
        //                                ],
        //                       has_more="0"

        // qDebug() << text;

        QString id_text = text.section('^', 0,0);

        if (id_text.toInt() == _variableId) {

            QString     children_text = Seer::parseFirst(text,     "children=", '[', ']', false);
            QStringList child_list    = Seer::parse(children_text, "child=",    '{', '}', false);
            QString     hasmore_text  = Seer::parseFirst(text,     "has_more=", '"', '"', false);

            for ( const auto& child_text : child_list ) {

                QString name_text     = Seer::parseFirst(child_text, "name=",      '"', '"', false);
                QString exp_text      = Seer::parseFirst(child_text, "exp=",       '"', '"', false);
                QString numchild_text = Seer::parseFirst(child_text, "numchild=",  '"', '"', false);
                QString value_text    = Seer::parseFirst(child_text, "value=",     '"', '"', false);
                QString type_text     = Seer::parseFirst(child_text, "type=",      '"', '"', false);
                QString threadid_text = Seer::parseFirst(child_text, "thread-id=", '"', '"', false);

                // Do we have an existing item to add to?
                QList<QTreeWidgetItem*> matches = variableTreeWidget->findItems(Seer::varObjParent(name_text), Qt::MatchExactly|Qt::MatchRecursive, 0);


                // No, just add to the top level.
                if (matches.size() == 0) {

                    qDebug() << name_text << "from the listchildren does not exist in the tree.";

                // Yes, add to it.
                }else{

                    QTreeWidgetItem* topItem = matches.takeFirst();

                    // Create the item.
                    QTreeWidgetItem* item = new QTreeWidgetItem;

                    item->setText(0, name_text);
                    item->setText(1, "");
                    item->setText(2, exp_text);
                    item->setText(3, numchild_text);
                    item->setText(4, Seer::filterEscapes(value_text));
                    item->setText(5, type_text);
                    item->setText(6, threadid_text);
                    item->setText(7, hasmore_text);

                    topItem->addChild(item);
                }

                // If there are children, get them.
                if (numchild_text != "0") {
                    emit varObjListChildren(_variableId, name_text);
                }
            }
        }


    }else if (text.contains(QRegExp("^([0-9]+)\\^done,changelist="))) {

        //
        // "4^done,changelist=[
        //                     {name=\"seer4.public.age\",                   value=\"60\",              in_scope=\"true\", type_changed=\"false\",                                        has_more=\"0\"},
        //                     {name=\"seer4.public.salary\",                value=\"0.25\",            in_scope=\"true\", type_changed=\"false\",                                        has_more=\"0\"},
        //                     {name=\"seer4.public.location.public.city\",  value=\"\\\"Houston\\\"\", in_scope=\"true\", type_changed=\"false\", displayhint=\"string\", dynamic=\"1\", has_more=\"0\"},
        //                     {name=\"seer4.public.location.public.state\", value=\"\\\"Texas\\\"\",   in_scope=\"true\", type_changed=\"false\", displayhint=\"string\", dynamic=\"1\", has_more=\"0\"},
        //                     {name=\"seer4.public.location.public.zip\",   value=\"77063\",           in_scope=\"true\", type_changed=\"false\",                                        has_more=\"0\"}
        //                    ]"

        // qDebug() << text;

        QString id_text = text.section('^', 0,0);

        if (id_text.toInt() == _variableId) {

            QString     changelist_text = Seer::parseFirst(text,       "changelist=", '[', ']', false);
            QStringList child_list      = Seer::parse(changelist_text, "",            '{', '}', false);

            for ( const auto& child_text : child_list ) {

                QString name_text         = Seer::parseFirst(child_text, "name=",          '"', '"', false);
                QString value_text        = Seer::parseFirst(child_text, "value=",         '"', '"', false);
                QString inscope_text      = Seer::parseFirst(child_text, "in_scope=",      '"', '"', false);
                QString typechanged_text  = Seer::parseFirst(child_text, "type_changed=",  '"', '"', false);
                QString displayhint_text  = Seer::parseFirst(child_text, "displayhint=",   '"', '"', false);
                QString dynamic_text      = Seer::parseFirst(child_text, "dynamic=",       '"', '"', false);
                QString hasmore_text      = Seer::parseFirst(child_text, "has_more=",      '"', '"', false);

                // Do we have an existing item to add to?
                QList<QTreeWidgetItem*> matches = variableTreeWidget->findItems(name_text, Qt::MatchExactly|Qt::MatchRecursive, 0);


                // No, just add to the top level.
                if (matches.size() == 0) {

                    qDebug() << name_text << "from the changelist does not exist in the tree.";

                // Yes, add to it.
                }else{

                    QTreeWidgetItem* item = matches.takeFirst();

                    item->setText(4, Seer::filterEscapes(value_text));
                    item->setText(7, hasmore_text);
                }
            }
        }


    }else if (text.contains(QRegExp("^([0-9]+)\\^error,msg="))) {

        qDebug() << text;

        QString id_text  = text.section('^', 0,0);
        QString msg_text = Seer::parseFirst(text, "msg=", '"', '"', false);

        if (id_text.toInt() == _variableId) {

            QTreeWidgetItem* topItem = variableTreeWidget->topLevelItem(0);
            if (topItem == 0) {
                return;
            }

            // Delete all subitems of the toplevel item.
            foreach (auto item, topItem->takeChildren()) delete item;

            // Set the error text.
            topItem->setText(1, "");
            topItem->setText(2, Seer::filterEscapes(msg_text));
        }


    }else if (text.startsWith("^error,msg=\"No registers.\"")) {
        variableTreeWidget->clear();

    // At a stopping point, refresh.
    }else if (text.startsWith("*stopped,reason=\"")) {

        if (autoRefreshCheckBox->isChecked()) {
            handleRefreshButton();
        }

    }else{
        // Ignore anything else.
    }

    // Resize columns.
    handleResizeColumns();

    // Set the cursor back.
    QApplication::restoreOverrideCursor();
}

void SeerVarVisualizerWidget::handleItemCreate (QTreeWidgetItem* parentItem, const QString& value_text) {

    qDebug() << value_text;

    if (Seer::hasBookends(value_text, '{', '}')) {

        // Remove bookends
        QString text = Seer::filterBookends(value_text, '{', '}');

        // Set the flatvalue text.
        parentItem->setText(1, "");
        parentItem->setText(2, Seer::filterEscapes(text));

        // Convert to a list of name/value pairs.
        QStringList nv_pairs = Seer::parseCommaList(text, '{', '}');

        // Go through each pair and add the name and its value to the tree.
        QTreeWidgetItem* prevItem = 0;

        for (const auto& nv : nv_pairs) {

            QStringPair pair = Seer::parseNameValue(nv, '=');

            // Handle "name = "xxxx", '\\000' <repeats 14 times>, "yyyy" ..., age = 0, salary = 0"
            // There is a 'first' value but no 'second'. So just concatenate unto the previous item.
            if (pair.second == "") {

                if (prevItem) {
                    prevItem->setText(0, prevItem->text(0) + pair.first);
                    prevItem->setText(1, "");
                    prevItem->setText(2, "");
                }

            // Normal case of "name = value".
            }else{

                QTreeWidgetItem* item = new QTreeWidgetItem;
                item->setText(0, pair.first);
                item->setText(1, "");
                item->setText(2, "");

                parentItem->addChild(item);

                // Handle recursion if value has bookends.
                if (Seer::hasBookends(pair.second, '{', '}')) {
                    handleItemCreate(item, pair.second);
                }else{
                    item->setText(1, "");
                    item->setText(2, Seer::filterEscapes(pair.second));
                }

                prevItem = item;
            }
        }

        parentItem->setExpanded(true);

    }else{
        parentItem->setText(1, "");
        parentItem->setText(2, Seer::filterEscapes(value_text));
    }
}

void SeerVarVisualizerWidget::handleContextMenu (const QPoint& pos) {

    QTreeWidgetItem* item = variableTreeWidget->itemAt(pos);

    if (item == 0) {
        return;
    }

    // Create the variable name.
    // If it's a struct, include its parent names.
    QString variable;

    QTreeWidgetItem* tmpItem = item->clone();
    while (tmpItem) {
        if (variable == "") {
            variable = tmpItem->text(0);
        }else{
            variable = tmpItem->text(0) + "." + variable;
        }

        tmpItem = tmpItem->parent();
    }

    // Create the menus.
    QAction* expandItemAction                     = new QAction(QString("Expand item"));
    QAction* collapseItemAction                   = new QAction(QString("Collapse item"));

    expandItemAction->setIcon(QIcon(":/seer/resources/RelaxLightIcons/list-add.svg"));
    collapseItemAction->setIcon(QIcon(":/seer/resources/RelaxLightIcons/list-remove.svg"));

    QAction* addMemoryVisualizerAction            = new QAction(QString("\"%1\"").arg(variable));
    QAction* addMemoryAsteriskVisualizerAction    = new QAction(QString("\"*%1\"").arg(variable));
    QAction* addMemoryAmpersandVisualizerAction   = new QAction(QString("\"&&%1\"").arg(variable));
    QAction* addArrayVisualizerAction             = new QAction(QString("\"%1\"").arg(variable));
    QAction* addArrayAsteriskVisualizerAction     = new QAction(QString("\"*%1\"").arg(variable));
    QAction* addArrayAmpersandVisualizerAction    = new QAction(QString("\"&&%1\"").arg(variable));
    QAction* addStructVisualizerAction            = new QAction(QString("\"%1\"").arg(variable));
    QAction* addStructAsteriskVisualizerAction    = new QAction(QString("\"*%1\"").arg(variable));
    QAction* addStructAmpersandVisualizerAction   = new QAction(QString("\"&&%1\"").arg(variable));

    // Are we on an item that has children?
    expandItemAction->setEnabled(false);
    collapseItemAction->setEnabled(false);

    if (item->childCount() > 0) {
        if (item->isExpanded() == false) {
            expandItemAction->setEnabled(true);
        }else{
            collapseItemAction->setEnabled(true);
        }
    }

    //expandItemAction->setEnabled(true);
    //collapseItemAction->setEnabled(true);

    // Populate the menu.
    QMenu menu("Visualizers", this);
    menu.setTitle("Visualizers");

    menu.addAction(expandItemAction);
    menu.addAction(collapseItemAction);
    menu.addSeparator();

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
    QAction* action = menu.exec(variableTreeWidget->viewport()->mapToGlobal(pos));

    // Do nothing.
    if (action == 0) {
        return;
    }

    // Handle expanding or collapsing tree.
    if (action == expandItemAction) {
        expandItem(item);

        // Resize columns.
        handleResizeColumns();
    }

    if (action == collapseItemAction) {
        collapseItem(item);

        // Resize columns.
        handleResizeColumns();
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

void SeerVarVisualizerWidget::handleItemEntered (QTreeWidgetItem* item, int column) {

    Q_UNUSED(column);

    item->setToolTip(0, item->text(0) + " : " + item->text(1) + " : " + item->text(2));

    for (int i=1; i<variableTreeWidget->columnCount(); i++) { // Copy tooltip to other columns.
        item->setToolTip(i, item->toolTip(0));
    }
}

void SeerVarVisualizerWidget::handleItemExpanded (QTreeWidgetItem* item) {

    Q_UNUSED(item);

    // Resize columns in a sec.
    // Have to schedule the resize later. Doing it immediatedly messes up the
    // tree display. Must be a Qt bug.
    QTimer::singleShot(200, this, &SeerVarVisualizerWidget::handleResizeColumns);
}

void SeerVarVisualizerWidget::handleResizeColumns () {

    // Resize columns.
    variableTreeWidget->resizeColumnToContents(0);
    variableTreeWidget->resizeColumnToContents(1);
    variableTreeWidget->resizeColumnToContents(2);
    variableTreeWidget->resizeColumnToContents(3);
    variableTreeWidget->resizeColumnToContents(4);
    variableTreeWidget->resizeColumnToContents(5);
    variableTreeWidget->resizeColumnToContents(6);
    variableTreeWidget->resizeColumnToContents(7);
}

void SeerVarVisualizerWidget::handleExpandAllButton () {

    variableTreeWidget->expandAll();

    // Resize columns.
    handleResizeColumns();
}

void SeerVarVisualizerWidget::handleCollapseAllButton () {

    variableTreeWidget->collapseAll();

    // Resize columns.
    handleResizeColumns();
}

void SeerVarVisualizerWidget::handleRefreshButton () {

    // Send signal to get variable result.
    if (_variableName != "") {
        emit varObjUpdate(_variableId, _variableName);
    }
}

void SeerVarVisualizerWidget::handleVariableNameLineEdit () {

    if (variableNameLineEdit->text() == "") {
        return;
    }

    setVariableName (variableNameLineEdit->text());
}

void SeerVarVisualizerWidget::writeSettings () {

    QSettings settings;

    settings.beginGroup("varvisualizerwindow");
    settings.setValue("size", size());
    settings.endGroup();
}

void SeerVarVisualizerWidget::readSettings () {

    QSettings settings;

    settings.beginGroup("varvisualizerwindow");
    resize(settings.value("size", QSize(800, 400)).toSize());
    settings.endGroup();
}

void SeerVarVisualizerWidget::resizeEvent (QResizeEvent* event) {

    writeSettings();

    QWidget::resizeEvent(event);
}

void SeerVarVisualizerWidget::expandItem (QTreeWidgetItem* item) {

    // If we're dealing with the top-level item, expand the tree the fast way.
    if (item == variableTreeWidget->topLevelItem(0)) {
        variableTreeWidget->expandAll();
        return;
    }

    // If this item has children, expand it. Then loop through
    // each child and expand them, recursively.
    if (item->childCount() > 0) {
        item->setExpanded(true);

        for (int i=0; i < item->childCount(); i++) {
            expandItem(item->child(i));
        }
    }
}

void SeerVarVisualizerWidget::collapseItem (QTreeWidgetItem* item) {

    // If we're dealing with the top-level item, collapse the tree the fast way.
    if (item == variableTreeWidget->topLevelItem(0)) {
        variableTreeWidget->collapseAll();
        return;
    }

    // If this item has children, collapse it. Then loop through
    // each child and collapse them, recursively.
    if (item->childCount() > 0) {
        item->setExpanded(false);

        for (int i=0; i < item->childCount(); i++) {
            collapseItem(item->child(i));
        }
    }
}

