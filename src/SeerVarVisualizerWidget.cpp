#include "SeerVarVisualizerWidget.h"
#include "SeerHelpPageDialog.h"
#include "SeerUtl.h"
#include "QEditDelegate.h"
#include <QtWidgets/QTreeWidget>
#include <QtWidgets/QTreeWidgetItemIterator>
#include <QtWidgets/QMenu>
#include <QAction>
#include <QtWidgets/QMessageBox>
#include <QtGui/QIcon>
#include <QtCore/QRegularExpression>
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
    setWindowTitle("Seer Struct Visualizer");
    setAttribute(Qt::WA_DeleteOnClose);

    variableNameLineEdit->setFocus();
    variableTreeWidget->setMouseTracking(true);
    variableTreeWidget->setSortingEnabled(false);
    variableTreeWidget->setContextMenuPolicy(Qt::CustomContextMenu);
    variableTreeWidget->setRootIsDecorated(true);
    variableTreeWidget->setItemsExpandable(true);
    variableTreeWidget->resizeColumnToContents(0); // variable
    variableTreeWidget->resizeColumnToContents(1); // value
    variableTreeWidget->resizeColumnToContents(2); // type
    variableTreeWidget->resizeColumnToContents(3); // varobj name
    variableTreeWidget->resizeColumnToContents(4); // varobj id
    variableTreeWidget->resizeColumnToContents(5); // exp
    variableTreeWidget->resizeColumnToContents(6); // numchild
    variableTreeWidget->resizeColumnToContents(7); // thread-id
    variableTreeWidget->resizeColumnToContents(8); // has_more
    variableTreeWidget->resizeColumnToContents(9); // editable
    variableTreeWidget->clear();

    // Show debug columns.
    debugCheckBox->setChecked(false);
    debugCheckBox->hide();

    // Create edit delegate.
    // The value column will allow editing of the cell. However, some cells can then
    // individually disable it again. ie: If the cell is for a node, not a value.
    // See 'new QTreeWidgetItem'
    QAllowEditDelegate* editDelegate = new QAllowEditDelegate(this);

    variableTreeWidget->setItemDelegateForColumn(0, new QNoEditDelegate(this));
    variableTreeWidget->setItemDelegateForColumn(1, editDelegate);
    variableTreeWidget->setItemDelegateForColumn(2, new QNoEditDelegate(this));
    variableTreeWidget->setItemDelegateForColumn(3, new QNoEditDelegate(this));
    variableTreeWidget->setItemDelegateForColumn(4, new QNoEditDelegate(this));
    variableTreeWidget->setItemDelegateForColumn(5, new QNoEditDelegate(this));
    variableTreeWidget->setItemDelegateForColumn(6, new QNoEditDelegate(this));
    variableTreeWidget->setItemDelegateForColumn(7, new QNoEditDelegate(this));
    variableTreeWidget->setItemDelegateForColumn(8, new QNoEditDelegate(this));
    variableTreeWidget->setItemDelegateForColumn(9, new QNoEditDelegate(this));


    // Connect things.
    QObject::connect(refreshToolButton,             &QToolButton::clicked,                       this,  &SeerVarVisualizerWidget::handleRefreshButton);
    QObject::connect(helpToolButton,                &QToolButton::clicked,                       this,  &SeerVarVisualizerWidget::handleHelpButton);
    QObject::connect(debugCheckBox,                 &QCheckBox::clicked,                         this,  &SeerVarVisualizerWidget::handleDebugCheckBox);
    QObject::connect(variableNameLineEdit,          &QLineEdit::returnPressed,                   this,  &SeerVarVisualizerWidget::handleVariableNameLineEdit);
    QObject::connect(variableTreeWidget,            &QTreeWidget::customContextMenuRequested,    this,  &SeerVarVisualizerWidget::handleContextMenu);
    QObject::connect(variableTreeWidget,            &QTreeWidget::itemEntered,                   this,  &SeerVarVisualizerWidget::handleItemEntered);
    QObject::connect(variableTreeWidget,            &QTreeWidget::itemExpanded,                  this,  &SeerVarVisualizerWidget::handleItemExpanded);
    QObject::connect(variableTreeWidget,            &QTreeWidget::itemCollapsed,                 this,  &SeerVarVisualizerWidget::handleItemCollapsed);
    QObject::connect(expandSelectedToolButton,      &QToolButton::clicked,                       this,  &SeerVarVisualizerWidget::handleExpandSelected);
    QObject::connect(collapseSelectedToolButton,    &QToolButton::clicked,                       this,  &SeerVarVisualizerWidget::handleCollapseSelected);
    QObject::connect(editDelegate,                  &QAllowEditDelegate::editingStarted,         this,  &SeerVarVisualizerWidget::handleIndexEditingStarted);
    QObject::connect(editDelegate,                  &QAllowEditDelegate::editingFinished,        this,  &SeerVarVisualizerWidget::handleIndexEditingFinished);

    // Show/hide columns.
    handleDebugCheckBox();

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

    setWindowTitle("Seer Struct Visualizer - '" + name + "'");

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

        item->setText(0, name);
        item->setText(1, "");
        item->setText(2, "");
        item->setText(3, "");
        item->setText(4, "");
        item->setText(5, "");
        item->setText(6, "");
        item->setText(7, "");
        item->setText(8, "");
        item->setText(9, "");

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

    if (text.contains(QRegularExpression("^([0-9]+)\\^done,name="))) {

        //
        // "-var-create x2112 "*" me"
        //
        // "4^done,name=\"seer4\",numchild=\"1\",value=\"{...}\",type=\"Person\",thread-id=\"1\",has_more=\"0\""
        //         --------------
        //

        QString id_text = text.section('^', 0,0);

        if (id_text.toInt() == _variableId) {

            //qDebug() << "var-create" << text;

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

            int varObjID = Seer::createID(); // Create id for queries.

            topItem->setText(1, Seer::filterEscapes(value_text));
            topItem->setText(2, type_text);
            topItem->setText(3, name_text);
            topItem->setText(4, QString::number(varObjID));
            topItem->setText(5, exp_text);
            topItem->setText(6, numchild_text);
            topItem->setText(7, threadid_text);
            topItem->setText(8, hasmore_text);
            topItem->setText(9, "");

            if (exp_text != "") {
                topItem->setText(0, exp_text);
            }

            //XXX Ask for its editable attributes.
            //XXX emit varObjAttributes (varObjID, name_text);

            // If there are children, add a placeholder.
            if (numchild_text != "0") {
                if (type_text.endsWith('*')) {
                    if (Seer::filterEscapes(value_text) != "0x0") {
                        // If it is a pointer that is not null, add a placeholder.  How universal is this for other languages?
                        QTreeWidgetItem* child = new QTreeWidgetItem;
                        child->setText(0, "{...}");
                        child->setText(3, name_text);

                        topItem->addChild(child);

                    }else{
                        deleteItems(topItem->takeChildren());
                    }

                }else{
                    // A non-pointer child, add a placeholder.
                    QTreeWidgetItem* child = new QTreeWidgetItem;
                    child->setText(0, "{...}");
                    child->setText(3, name_text);

                    topItem->addChild(child);
                }

            }

            // Save the VarObj name.
            _variableName = name_text;
        }


    }else if (text.contains(QRegularExpression("^([0-9]+)\\^done,numchild="))) {

        //
        // "-var-list-children --all-values x2112.public"
        //
        // "4^done,numchild="1", children=[
        //         ------------
        //                                 child={name="x2112.public",exp="public",numchild="4",thread-id="1"} 
        //                                ],
        //                       has_more="0"
        //

        QString id_text = text.section('^', 0,0);

        if (id_text.toInt() == _variableId) {

            //qDebug() << "var-list-children" << text;

            QString     children_text = Seer::parseFirst(text,     "children=", '[', ']', false);
            QStringList child_list    = Seer::parse(children_text, "child=",    '{', '}', false);
            QString     hasmore_text  = Seer::parseFirst(text,     "has_more=", '"', '"', false);

            for (const auto& child_text : child_list) {

                QString name_text     = Seer::parseFirst(child_text, "name=",      '"', '"', false);
                QString exp_text      = Seer::parseFirst(child_text, "exp=",       '"', '"', false);
                QString numchild_text = Seer::parseFirst(child_text, "numchild=",  '"', '"', false);
                QString value_text    = Seer::parseFirst(child_text, "value=",     '"', '"', false);
                QString type_text     = Seer::parseFirst(child_text, "type=",      '"', '"', false);
                QString threadid_text = Seer::parseFirst(child_text, "thread-id=", '"', '"', false);

                // Do we have an existing item to add to?
                QTreeWidgetItem* matchItem = findItem(Seer::varObjParent(name_text), Qt::MatchExactly|Qt::MatchRecursive, 3);

                // No, just add to the top level.
                if (matchItem == 0) {

                    // This shouldn't really happen.
                    qDebug() << name_text << "from the listchildren does not exist in the tree.";

                // Yes, add to it.
                }else{

                    // See if item has a "{...}" child. If so, remove it and any other siblings.
                    if (matchItem->childCount() > 0) {
                        QTreeWidgetItem* childItem = matchItem->child(0);
                        if (childItem->text(0) == "{...}") {
                            deleteItems(matchItem->takeChildren());
                        }
                    }

                    // Create the item.
                    QTreeWidgetItem* item = new QTreeWidgetItem;

                    int varObjID = Seer::createID(); // Create id for queries.

                    item->setText(0, "");
                    item->setText(1, Seer::filterEscapes(value_text));
                    item->setText(2, type_text);
                    item->setText(3, name_text);
                    item->setText(4, QString::number(varObjID));
                    item->setText(5, exp_text);
                    item->setText(6, numchild_text);
                    item->setText(7, threadid_text);
                    item->setText(8, hasmore_text);
                    item->setText(9, "");
                    item->setFlags(item->flags() | Qt::ItemIsEditable); //XXX Set item editable if it can.

                    if (exp_text != "") {
                        item->setText(0, exp_text);
                    }

                    //XXX Ask for its editable attributes.
                    //XXX emit varObjAttributes (varObjID, name_text);

                    // If there are children, add a placeholder.
                    bool expandItem = false;

                    if (numchild_text != "0") {
                        if (type_text.endsWith('*')) {
                            if (Seer::filterEscapes(value_text) != "0x0") {
                                // If it is a pointer that is not null, add a placeholder.  How universal is this for other languages?
                                QTreeWidgetItem* child = new QTreeWidgetItem;
                                child->setText(0, "{...}");
                                child->setText(3, name_text);

                                item->addChild(child);

                            }else{
                                deleteItems(item->takeChildren());
                            }

                        }else{
                            // A non-pointer child, add a placeholder.
                            QTreeWidgetItem* child = new QTreeWidgetItem;
                            child->setText(0, "{...}");
                            child->setText(3, name_text);

                            item->addChild(child);

                            expandItem = true;
                        }
                    }

                    matchItem->addChild(item);

                    if (expandItem == true && expandRecursiveCheckBox->isChecked()) {
                        item->setExpanded(true);
                    }
                }
            }
        }


    }else if (text.contains(QRegularExpression("^([0-9]+)\\^done,changelist="))) {

        //qDebug() << "var-update-children" << text;

        //
        // "4^done,changelist=[
        //                     {name=\"seer4.public.age\",                   value=\"60\",              in_scope=\"true\", type_changed=\"false\",                                        has_more=\"0\"},
        //                     {name=\"seer4.public.salary\",                value=\"0.25\",            in_scope=\"true\", type_changed=\"false\",                                        has_more=\"0\"},
        //                     {name=\"seer4.public.location.public.city\",  value=\"\\\"Houston\\\"\", in_scope=\"true\", type_changed=\"false\", displayhint=\"string\", dynamic=\"1\", has_more=\"0\"},
        //                     {name=\"seer4.public.location.public.state\", value=\"\\\"Texas\\\"\",   in_scope=\"true\", type_changed=\"false\", displayhint=\"string\", dynamic=\"1\", has_more=\"0\"},
        //                     {name=\"seer4.public.location.public.zip\",   value=\"77063\",           in_scope=\"true\", type_changed=\"false\",                                        has_more=\"0\"}
        //                    ]"

        QString id_text = text.section('^', 0,0);

        if (id_text.toInt() == _variableId) {

            QString     changelist_text = Seer::parseFirst(text,       "changelist=", '[', ']', false);
            QStringList child_list      = Seer::parse(changelist_text, "",            '{', '}', false);

            for (const auto& child_text : child_list) {

                QString name_text         = Seer::parseFirst(child_text, "name=",          '"', '"', false);
                QString value_text        = Seer::parseFirst(child_text, "value=",         '"', '"', false);
                QString inscope_text      = Seer::parseFirst(child_text, "in_scope=",      '"', '"', false);
                QString typechanged_text  = Seer::parseFirst(child_text, "type_changed=",  '"', '"', false);
                QString displayhint_text  = Seer::parseFirst(child_text, "displayhint=",   '"', '"', false);
                QString dynamic_text      = Seer::parseFirst(child_text, "dynamic=",       '"', '"', false);
                QString hasmore_text      = Seer::parseFirst(child_text, "has_more=",      '"', '"', false);

                // Do we have an existing item to add to?
                QTreeWidgetItem* matchItem = findItem(name_text, Qt::MatchExactly|Qt::MatchRecursive, 3);

                // No, just add to the top level.
                if (matchItem == 0) {

                    qDebug() << name_text << "from the changelist does not exist in the tree.";

                // Yes, add to it.
                }else{

                    matchItem->setText(1, Seer::filterEscapes(value_text));
                    matchItem->setText(7, hasmore_text);

                    // If there are children, add a placeholder.
                    if (matchItem->text(5) != "0") {
                        if (matchItem->text(2).endsWith('*')) {
                            if (Seer::filterEscapes(matchItem->text(1)) != "0x0") {
                                // If it is a pointer that is not null, add a placeholder.  How universal is this for other languages?
                                QTreeWidgetItem* child = new QTreeWidgetItem;
                                child->setText(0, "{...}");
                                child->setText(3, matchItem->text(3));

                                matchItem->addChild(child);

                            }else{
                                deleteItems(matchItem->takeChildren());
                            }

                        }else{
                            // A non-pointer child, add a placeholder.
                            QTreeWidgetItem* child = new QTreeWidgetItem;
                            child->setText(0, "{...}");
                            child->setText(3, matchItem->text(3));

                            matchItem->addChild(child);
                        }
                    }

                    // Need to refresh?
                    if (typechanged_text == "true") {
                        handleRefreshButton();
                    }
                }
            }
        }


    }else if (text.contains(QRegularExpression("^([0-9]+)\\^done,attr="))) {

        //qDebug() << "var-show-attributes" << text;

        // 4^done,attr="noneditable"
        // 4^done,attr="editable"

        QString id_text       = text.section('^', 0,0);
        QString editable_text = Seer::parseFirst(text, "attr=",  '"', '"', false);

        // Do we have any items that match the varobjid?
        QTreeWidgetItem* matchItem = findItem(id_text, Qt::MatchExactly|Qt::MatchRecursive, 4);

        if (matchItem) {
            matchItem->setText(9, editable_text);

            if (editable_text == "editable") {
                matchItem->setFlags(matchItem->flags() | Qt::ItemIsEditable);
            }else{
                matchItem->setFlags(matchItem->flags() & ~Qt::ItemIsEditable);
            }
        }


    }else if (text.contains(QRegularExpression("^([0-9]+)\\^error,msg="))) {

        QString id_text  = text.section('^', 0,0);
        QString msg_text = Seer::parseFirst(text, "msg=", '"', '"', false);

        if (id_text.toInt() == _variableId) {

            QMessageBox::warning(this, "Seer",
                                 QString("Visualizer Error: '%1'\n").arg(Seer::filterEscapes(msg_text)),
                                 QMessageBox::Ok);

        }

        if (msg_text.contains("Variable object is not editable")) {

            if (_previousEditName != "") {

                QTreeWidgetItem* matchItem = findItem(_previousEditName, Qt::MatchExactly|Qt::MatchRecursive, 3);
                if (matchItem) {
                    matchItem->setText(1, _previousEditValue);
                }
            }
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
}

void SeerVarVisualizerWidget::handleContextMenu (const QPoint& pos) {

    QTreeWidgetItem* item = variableTreeWidget->itemAt(pos);

    if (item == 0) {
        return;
    }

    // Create the variable name.
    // If it's a struct, include its parent names.
    QString variable = fullVariableName(item);

    // Create the menus.
    QAction* expandItemAction                     = new QAction(QString("Expand item"));
    QAction* collapseItemAction                   = new QAction(QString("Collapse item"));
    QAction* addMemoryVisualizerAction            = new QAction(QString("\"%1\"").arg(variable));
    QAction* addMemoryAsteriskVisualizerAction    = new QAction(QString("\"*%1\"").arg(variable));
    QAction* addMemoryAmpersandVisualizerAction   = new QAction(QString("\"&&%1\"").arg(variable));
    QAction* addArrayVisualizerAction             = new QAction(QString("\"%1\"").arg(variable));
    QAction* addArrayAsteriskVisualizerAction     = new QAction(QString("\"*%1\"").arg(variable));
    QAction* addArrayAmpersandVisualizerAction    = new QAction(QString("\"&&%1\"").arg(variable));
    QAction* addVarVisualizerAction               = new QAction(QString("\"%1\"").arg(variable));
    QAction* addVarAsteriskVisualizerAction       = new QAction(QString("\"*%1\"").arg(variable));
    QAction* addVarAmpersandVisualizerAction      = new QAction(QString("\"&&%1\"").arg(variable));

    expandItemAction->setIcon(QIcon(":/seer/resources/RelaxLightIcons/list-add.svg"));
    collapseItemAction->setIcon(QIcon(":/seer/resources/RelaxLightIcons/list-remove.svg"));

    // Are we on an item that has children?
    expandItemAction->setEnabled(false);
    collapseItemAction->setEnabled(false);

    expandItemAction->setText(QString("Expand item '%1'").arg(item->text(0)));
    collapseItemAction->setText(QString("Collapse item '%1'").arg(item->text(0)));

    if (item->childCount() > 0) {
        if (item->isExpanded() == false) {
            expandItemAction->setEnabled(true);
        }else{
            collapseItemAction->setEnabled(true);
        }
    }

    // Populate the menu.
    QMenu menu("Visualizers", this);
    menu.setTitle("Visualizers");

    menu.addAction(expandItemAction);
    menu.addAction(collapseItemAction);

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
    structVisualizerMenu.addAction(addVarVisualizerAction);
    structVisualizerMenu.addAction(addVarAsteriskVisualizerAction);
    structVisualizerMenu.addAction(addVarAmpersandVisualizerAction);
    menu.addMenu(&structVisualizerMenu);

    // Launch the menu. Get the response.
    QAction* action = menu.exec(variableTreeWidget->viewport()->mapToGlobal(pos));

    // Do nothing.
    if (action == 0) {
        return;
    }

    // Handle expanding the current item.
    if (action == expandItemAction) {

        expandItem(item);

        return;
    }

    // Handle collapsing the current item.
    if (action == collapseItemAction) {

        collapseItem(item);

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
    if (action == addVarVisualizerAction) {

        //qDebug() << "addVarVisualizer" << variable;

        // Emit the signals.
        if (variable != "") {
            emit addVarVisualize(variable);
        }

        return;
    }

    // Handle adding struct to visualize.
    if (action == addVarAsteriskVisualizerAction) {

        //qDebug() << "addVarAsteriskVisualizer" << variable;

        // Emit the signals.
        if (variable != "") {
            emit addVarVisualize(QString("*") + variable);
        }

        return;
    }

    // Handle adding struct to visualize.
    if (action == addVarAmpersandVisualizerAction) {

        //qDebug() << "addVarAmpersandVisualizer" << variable;

        // Emit the signals.
        if (variable != "") {
            emit addVarVisualize(QString("&") + variable);
        }

        return;
    }
}

void SeerVarVisualizerWidget::handleItemEntered (QTreeWidgetItem* item, int column) {

    Q_UNUSED(column);

    QString text = toolTipText(item);

    if (text != "") {
        for (int i=0; i<variableTreeWidget->columnCount(); i++) { // Set tooltip to all columns.
            item->setToolTip(i, text);
        }

        // Ask for its editable attributes.
        if (item->text(4) != "") {
            //XXX emit varObjAttributes (item->text(4).toInt(), item->text(3));
        }
    }
}

void SeerVarVisualizerWidget::handleItemExpanded (QTreeWidgetItem* item) {

    // Ask for the children.
    emit varObjListChildren(_variableId, item->text(3));

    // Resize columns in a sec.
    // Have to schedule the resize later. Doing it immediatedly messes up the
    // tree display. Must be a Qt bug.
    QTimer::singleShot(100, this, &SeerVarVisualizerWidget::handleResizeColumns);
}

void SeerVarVisualizerWidget::handleItemCollapsed (QTreeWidgetItem* item) {

    // Delete all children items.
    deleteItems(item->takeChildren());

    // If there are children, add a placeholder.
    if (item->text(5) != "0") {
        if (item->text(2).endsWith('*')) {
            if (Seer::filterEscapes(item->text(1)) != "0x0") {
                // If it is a pointer that is not null, add a placeholder.  How universal is this for other languages?
                QTreeWidgetItem* child = new QTreeWidgetItem;
                child->setText(0, "{...}");
                child->setText(3, item->text(3));

                item->addChild(child);
            }

        }else{
            // A non-pointer child, add a placeholder.
            QTreeWidgetItem* child = new QTreeWidgetItem;
            child->setText(0, "{...}");
            child->setText(3, item->text(3));

            item->addChild(child);
        }
    }

    // Resize columns in a sec.
    // Have to schedule the resize later. Doing it immediatedly messes up the
    // tree display. Must be a Qt bug.
    QTimer::singleShot(100, this, &SeerVarVisualizerWidget::handleResizeColumns);
}

void SeerVarVisualizerWidget::handleExpandSelected () {

    // Expand the current item, if selected.
    if (variableTreeWidget->currentItem()) {
        expandItem(variableTreeWidget->currentItem());
        return;
    }

    // Or the entire tree.
    if (variableTreeWidget->topLevelItemCount() > 0) {
        expandItem(variableTreeWidget->topLevelItem(0));
    }
}

void SeerVarVisualizerWidget::handleCollapseSelected () {

    // Collapse the current item, if selected.
    if (variableTreeWidget->currentItem()) {
        collapseItem(variableTreeWidget->currentItem());
        return;
    }

    // Or the entire tree.
    if (variableTreeWidget->topLevelItemCount() > 0) {
        collapseItem(variableTreeWidget->topLevelItem(0));
    }
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
    variableTreeWidget->resizeColumnToContents(8);
    variableTreeWidget->resizeColumnToContents(9);
}

void SeerVarVisualizerWidget::handleHideDebugColumns (bool flag) {

    variableTreeWidget->setColumnHidden(0, false);
    variableTreeWidget->setColumnHidden(1, false);
    variableTreeWidget->setColumnHidden(2, false);
    variableTreeWidget->setColumnHidden(3, flag);
    variableTreeWidget->setColumnHidden(4, flag);
    variableTreeWidget->setColumnHidden(5, flag);
    variableTreeWidget->setColumnHidden(6, flag);
    variableTreeWidget->setColumnHidden(7, flag);
    variableTreeWidget->setColumnHidden(8, flag);
    variableTreeWidget->setColumnHidden(9, flag);

    handleResizeColumns();
}

void SeerVarVisualizerWidget::handleRefreshButton () {

    // Send signal to get variable result.
    if (_variableName != "") {
        emit varObjUpdate(_variableId, _variableName);
    }
}

void SeerVarVisualizerWidget::handleHelpButton () {

    SeerHelpPageDialog* help = new SeerHelpPageDialog;
    help->loadFile(":/seer/resources/help/StructVisualizer.md");
    help->show();
    help->raise();
}

void SeerVarVisualizerWidget::handleDebugCheckBox () {

    handleHideDebugColumns(!debugCheckBox->isChecked());
}

void SeerVarVisualizerWidget::handleVariableNameLineEdit () {

    if (variableNameLineEdit->text() == "") {
        return;
    }

    setVariableName (variableNameLineEdit->text());
}

void SeerVarVisualizerWidget::handleIndexEditingStarted (const QModelIndex& index) {

    _previousEditName  = "";
    _previousEditValue = "";

    QTreeWidgetItem* item = variableTreeWidget->getItemFromIndex(index);

    if (item == 0) {
        return;
    }

    // Get the old value;
    _previousEditName  = item->text(3);
    _previousEditValue = item->text(1);
}

void SeerVarVisualizerWidget::handleIndexEditingFinished (const QModelIndex& index) {

    QTreeWidgetItem* item = variableTreeWidget->getItemFromIndex(index);

    if (item == 0) {
        return;
    }

    // Get the new value;
    QString value = item->text(1);

    // If it hasn't changed, we do need to bother.
    if (value == _previousEditValue) {

        _previousEditName  = "";
        _previousEditValue = "";

        return;
    }

    // Emit the signal to change the varobj to the new value.
    emit varObjAssign(_variableId, item->text(3), value);
}

QTreeWidgetItem* SeerVarVisualizerWidget::findItem (const QString& text, Qt::MatchFlags flags, int column) {

    QList<QTreeWidgetItem*> matches = variableTreeWidget->findItems(text, flags, column);

    if (matches.size() == 0) {
        return nullptr;
    }

    return matches[0];
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

    if (item->childCount() == 0) {
        return;
    }

    if (item->isExpanded() == false) {
        item->setExpanded(true);
    }
}

void SeerVarVisualizerWidget::collapseItem (QTreeWidgetItem* item) {

    if (item->childCount() == 0) {
        return;
    }

    if (item->isExpanded() == true) {
        item->setExpanded(false);
    }
}

QString SeerVarVisualizerWidget::fullVariableName (QTreeWidgetItem* item) {

    // If the item has no value or type, then return no variable.
    if (item->text(1) == "" || item->text(2) == "") {
        return "";
    }

    // Otherwise traverse up the tree to build up a variable name string, which includes '.' or '->'
    // depending if the type ends with a '*'.
    QString          name;
    QTreeWidgetItem* tmp = item;

    while (tmp) {

        // Skip items that have no value or type. 'public', 'private', 'protected'.
        if (tmp->text(1) == "" || tmp->text(2) == "" || (tmp->text(0) == tmp->text(2))) {
            tmp = tmp->parent();
            continue;
        }

        // Add the variable part to the variable name.
        if (tmp->childCount() > 0) {
            if (tmp->text(2).endsWith('*')) {
                if (name == "") {
                    name.prepend(tmp->text(0));
                }else{
                    name.prepend(tmp->text(0) + "->");
                }
            }else{
                if (name == "") {
                    name.prepend(tmp->text(0));
                }else{
                    name.prepend(tmp->text(0) + ".");
                }
            }
        }else{
            name.prepend(tmp->text(0));
        }

        // Move to the parent to get the type, if we can.
        tmp = tmp->parent();
    }

    return name;
}

QString SeerVarVisualizerWidget::toolTipText (QTreeWidgetItem* item) {

    // If the item has no value or type, then return no tooltip.
    if (item->text(1) == "" || item->text(2) == "") {
        return "";
    }

    // Get the full variable name.
    QString name = fullVariableName(item);

    if (name == "") {
        return name;
    }

    // Tack on the variable value and type.
    QString text = name + " : " + item->text(1) + " : " + item->text(2);

    return text;
}

void SeerVarVisualizerWidget::debug (QString message,  QTreeWidgetItem* item) {

    // 0 variable
    // 1 value
    // 2 type
    // 3 varobj name
    // 4 varobj id
    // 5 exp
    // 6 numchild
    // 7 thread-id
    // 8 has_more
    // 8 editable

    if (item == 0) {
        qDebug() << message << "NULL";
    }else{
        qDebug() << message << item->text(0) << item->text(1) << item->text(2) << item->text(3) << item->text(4) << item->text(5) << item->text(6) << item->text(7) << item->text(8) << item->text(9);
    }
}

void SeerVarVisualizerWidget::deleteItems (QList<QTreeWidgetItem*> items) {

    // Loop through the list and delete the items.
    // Note, the list will still contain pointers, but they will be invalid.
    foreach (auto item, items) delete item;
}

