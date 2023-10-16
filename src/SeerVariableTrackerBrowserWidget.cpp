#include "SeerVariableTrackerBrowserWidget.h"
#include "SeerUtl.h"
#include <QtWidgets/QTreeWidget>
#include <QtWidgets/QTreeWidgetItemIterator>
#include <QtWidgets/QApplication>
#include <QtWidgets/QMenu>
#include <QtGui/QFontDatabase>
#include <QtGui/QClipboard>
#include <QtCore/QTimer>
#include <QtCore/QDebug>
#include <iostream>

SeerVariableTrackerBrowserWidget::SeerVariableTrackerBrowserWidget (QWidget* parent) : QWidget(parent) {

    // Construct the UI.
    setupUi(this);

    // Setup the widgets
    variablesTreeWidget->setMouseTracking(true);
    variablesTreeWidget->setSortingEnabled(false);
    variablesTreeWidget->setContextMenuPolicy(Qt::CustomContextMenu);
    variablesTreeWidget->setSelectionMode(QAbstractItemView::ExtendedSelection);
    variablesTreeWidget->resizeColumnToContents(0); // id
    variablesTreeWidget->resizeColumnToContents(1); // name
    variablesTreeWidget->resizeColumnToContents(2); // value
    variablesTreeWidget->setColumnHidden(0, true); // Hide the 'number' column.
    variablesTreeWidget->clear();

    // Connect things.
    QObject::connect(variableAddLineEdit,            &QLineEdit::returnPressed,                     this, &SeerVariableTrackerBrowserWidget::handleAddLineEdit);
    QObject::connect(variableDeleteToolButton,       &QToolButton::clicked,                         this, &SeerVariableTrackerBrowserWidget::handleDeleteToolButton);
    QObject::connect(variableDeleteAllToolButton,    &QToolButton::clicked,                         this, &SeerVariableTrackerBrowserWidget::handleDeleteAllToolButton);
    QObject::connect(variablesTreeWidget,            &QTreeWidget::itemEntered,                     this, &SeerVariableTrackerBrowserWidget::handleItemEntered);
    QObject::connect(variablesTreeWidget,            &QTreeWidget::customContextMenuRequested,      this, &SeerVariableTrackerBrowserWidget::handleContextMenu);
}

SeerVariableTrackerBrowserWidget::~SeerVariableTrackerBrowserWidget () {
}

void SeerVariableTrackerBrowserWidget::handleText (const QString& text) {

    // Don't do any work if the widget is hidden.
    if (isHidden()) {
        return;
    }

    QApplication::setOverrideCursor(Qt::BusyCursor);

    if (text.startsWith("^done,DataExpressionTable={") && text.endsWith("}")) {

        //qDebug() << text;

        // "^done,DataExpressionTable={
        //          entry={id=\"1\",expression=\"s\"},
        //          entry={id=\"2\",expression=\"v\"},
        //          entry={id=\"4\",expression=\"l\"},
        //          entry={id=\"5\",expression=\"m\"}
        //      }"

        variablesTreeWidget->clear();

        QString frame_text = Seer::parseFirst(text, "DataExpressionTable=", '{', '}', false);

        QStringList entries_list = Seer::parse(frame_text, "entry=", '{', '}', false);

        for (int i=0; i<entries_list.size(); i++) {

            QString entry_text = entries_list[i];

            QString id_text         = Seer::parseFirst(entry_text, "id=",         '"', '"', false);
            QString expression_text = Seer::parseFirst(entry_text, "expression=", '"', '"', false);

            QTreeWidgetItem* topItem = new QTreeWidgetItem;
            topItem->setText(0, id_text);
            topItem->setText(1, expression_text);
            topItem->setText(2, "");

            topItem->setFont(2, QFontDatabase::systemFont(QFontDatabase::FixedFont));

            variablesTreeWidget->addTopLevelItem(topItem);
        }

    }else if (text.startsWith("^done,DataExpressionAdded={") && text.endsWith("}")) {

        //qDebug() << "Refresh";

        // "^done,DataExpressionAdded={
        //          id=\"5\",
        //          expression=\"m\"
        //      }"

        QString frame_text = Seer::parseFirst(text, "DataExpressionAdded=", '{', '}', false);

        QString id_text         = Seer::parseFirst(frame_text, "id=",         '"', '"', false);
        QString expression_text = Seer::parseFirst(frame_text, "expression=", '"', '"', false);

        QTreeWidgetItem* topItem = new QTreeWidgetItem;
        topItem->setText(0, id_text);
        topItem->setText(1, expression_text);
        topItem->setText(2, "");

        topItem->setFont(2, QFontDatabase::systemFont(QFontDatabase::FixedFont));

        variablesTreeWidget->addTopLevelItem(topItem);

    }else if (text.startsWith("^done,DataExpressionDeleted={") && text.endsWith("}")) {

        //qDebug() << text;

        // "^done,DataExpressionDeleted={
        //          entry={id=\"1\",expression=\"s\"},
        //          entry={id=\"3\",expression=\"vb\"}
        //      }"

        QString frame_text = Seer::parseFirst(text, "DataExpressionDeleted=", '{', '}', false);

        QStringList entries_list = Seer::parse(frame_text, "entry=", '{', '}', false);

        for (int i=0; i<entries_list.size(); i++) {

            QString entry_text = entries_list[i];

            QString id_text         = Seer::parseFirst(entry_text, "id=",         '"', '"', false);
            QString expression_text = Seer::parseFirst(entry_text, "expression=", '"', '"', false);

            QList<QTreeWidgetItem*> matches = variablesTreeWidget->findItems(id_text, Qt::MatchExactly, 0);

            qDeleteAll(matches);
        }

    }else if (text.contains(QRegularExpression("^([0-9]+)\\^done,value="))) {

        //qDebug() << text;

        // "6^done,value=\"\\\"abc\\\"\""

        QString id_text    = text.section('^', 0,0);
        QString value_text = Seer::parseFirst(text, "value=", '"', '"', false);

        QList<QTreeWidgetItem*> matches = variablesTreeWidget->findItems(id_text, Qt::MatchExactly, 0);

        if (matches.size() > 0) {
            matches.first()->setText(2, Seer::filterEscapes(value_text));
        }

    }else if (text.contains(QRegularExpression("^([0-9]+)\\^error,msg="))) {

        //qDebug() << text;

        // "1^error,msg=\"No symbol \\\"j\\\" in current context.\""

        QString id_text  = text.section('^', 0,0);
        QString msg_text = Seer::parseFirst(text, "msg=", '"', '"', false);

        QList<QTreeWidgetItem*> matches = variablesTreeWidget->findItems(id_text, Qt::MatchExactly, 0);

        if (matches.size() > 0) {
            matches.first()->setText(2, Seer::filterEscapes(msg_text));
        }

    }else if (text.startsWith("^error,msg=\"No registers.\"")) {
        variablesTreeWidget->clear();

    }else{
        // Ignore others.
    }

    variablesTreeWidget->resizeColumnToContents(0);
    variablesTreeWidget->resizeColumnToContents(1);
    variablesTreeWidget->resizeColumnToContents(2);

    QApplication::restoreOverrideCursor();
}

void SeerVariableTrackerBrowserWidget::handleStoppingPointReached () {

    // Don't do any work if the widget is hidden.
    if (isHidden()) {
        return;
    }

    refresh();
}

void SeerVariableTrackerBrowserWidget::refresh () {
    emit refreshVariableTrackerNames();
    emit refreshVariableTrackerValues();
}

void SeerVariableTrackerBrowserWidget::refreshValues () {
    emit refreshVariableTrackerValues();
}

void SeerVariableTrackerBrowserWidget::handleAddLineEdit () {

    //qDebug();

    QString variable = variableAddLineEdit->text();

    variableAddLineEdit->clear();

    if (variable != "") {
        emit addVariableExpression(variable);

        // After sending the 'add' signal, schedule a 'refresh' 200ms later.
        QTimer::singleShot(200, this, &SeerVariableTrackerBrowserWidget::refresh);
    }
}

void SeerVariableTrackerBrowserWidget::handleDeleteToolButton () {

    //qDebug();

    // Get selected tree items.
    QList<QTreeWidgetItem*> items = variablesTreeWidget->selectedItems();

    // Build a string that is a list of variable ids.
    QString variableids;

    QList<QTreeWidgetItem*>::iterator i;
    for (i = items.begin(); i != items.end(); ++i) {

        if (i != items.begin()) {
            variableids += " ";
        }

        variableids += (*i)->text(0);
    }

    // Don't do anything if the list of variables is empty.
    if (variableids == "") {
        return;
    }

    // Send the signal.
    emit deleteVariableExpressions(variableids);
}

void SeerVariableTrackerBrowserWidget::handleDeleteAllToolButton () {
    emit deleteVariableExpressions("*");
}

void SeerVariableTrackerBrowserWidget::handleItemEntered (QTreeWidgetItem* item, int column) {

    Q_UNUSED(column);

    //qDebug() << item->text(0) << column;

    item->setToolTip(0, item->text(1) + " : " + item->text(2));

    for (int i=1; i<variablesTreeWidget->columnCount(); i++) { // Copy tooltip to other columns.
        item->setToolTip(i, item->toolTip(0));
    }
}

void SeerVariableTrackerBrowserWidget::showEvent (QShowEvent* event) {

    QWidget::showEvent(event);

    refresh();
}

void SeerVariableTrackerBrowserWidget::handleContextMenu (const QPoint& pos) {

    // Get the item at the cursor.
    QTreeWidgetItem* item = variablesTreeWidget->itemAt(pos);

    // Construct the menu.
    QMenu*   menu          = new QMenu("Options", this);
    QAction* copyAction    = menu->addAction("Copy selected");
    QAction* copyAllAction = menu->addAction("Copy all");

    // If no selected item, disable 'selected' copy but allow 'all'.
    if (item == 0) {
        copyAction->setEnabled(false);
    }

    // Execute the menu. Return if nothing.
    QAction* action = menu->exec(variablesTreeWidget->mapToGlobal(pos));

    if (action == 0) {
        return;
    }

    // Get selected tree items.
    QList<QTreeWidgetItem*> items;

    // Get list of 'select' items.
    if (action == copyAction) {
        items = variablesTreeWidget->selectedItems();
    }

    // Get list of 'all' items.
    if (action == copyAllAction) {
        items = variablesTreeWidget->findItems(QString("*"), Qt::MatchWrap | Qt::MatchWildcard);
    }

    // Populate the clipboard.
    if (items.size() == 0) {
        return;
    }

    QClipboard* clipboard = QGuiApplication::clipboard();

    QString text;

    for (int i=0; i<items.size(); i++) {

        if (i != 0) {
            text += '\n';
        }

        text += items[i]->text(1) + ":" + items[i]->text(2);
    }

    clipboard->setText(text, QClipboard::Clipboard);
    clipboard->setText(text, QClipboard::Selection);
}

