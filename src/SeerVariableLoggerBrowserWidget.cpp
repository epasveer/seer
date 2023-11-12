#include "SeerVariableLoggerBrowserWidget.h"
#include "SeerUtl.h"
#include <QtWidgets/QTreeWidget>
#include <QtWidgets/QTreeWidgetItemIterator>
#include <QtWidgets/QApplication>
#include <QtWidgets/QMenu>
#include <QtGui/QFontDatabase>
#include <QtGui/QClipboard>
#include <QtCore/QTime>
#include <QtCore/QDebug>
#include <QAction>

SeerVariableLoggerBrowserWidget::SeerVariableLoggerBrowserWidget (QWidget* parent) : QWidget(parent) {

    // Construct the UI.
    setupUi(this);

    // Setup the widgets
    variablesTreeWidget->setMouseTracking(true);
    variablesTreeWidget->setSortingEnabled(false);
    variablesTreeWidget->setContextMenuPolicy(Qt::CustomContextMenu);
    variablesTreeWidget->setSelectionMode(QAbstractItemView::ExtendedSelection);
    variablesTreeWidget->resizeColumnToContents(0); // id
    variablesTreeWidget->resizeColumnToContents(1); // timestamp
    variablesTreeWidget->resizeColumnToContents(2); // name
    variablesTreeWidget->resizeColumnToContents(3); // value
    variablesTreeWidget->setColumnHidden(0, true);  // Hide the 'id' column.
    variablesTreeWidget->clear();

    // Connect things.
    QObject::connect(this,                           &SeerVariableLoggerBrowserWidget::evaluateVariableExpression,      this, &SeerVariableLoggerBrowserWidget::handleEvaluateVariableExpression);
    QObject::connect(variableAddLineEdit,            &QLineEdit::returnPressed,                                         this, &SeerVariableLoggerBrowserWidget::handleAddLineEdit);
    QObject::connect(variableDeleteToolButton,       &QToolButton::clicked,                                             this, &SeerVariableLoggerBrowserWidget::handleDeleteToolButton);
    QObject::connect(variableDeleteAllToolButton,    &QToolButton::clicked,                                             this, &SeerVariableLoggerBrowserWidget::handleDeleteAllToolButton);
    QObject::connect(variablesTreeWidget,            &QTreeWidget::itemEntered,                                         this, &SeerVariableLoggerBrowserWidget::handleItemEntered);
    QObject::connect(variablesTreeWidget,            &QTreeWidget::customContextMenuRequested,                          this, &SeerVariableLoggerBrowserWidget::handleContextMenu);
}

SeerVariableLoggerBrowserWidget::~SeerVariableLoggerBrowserWidget () {
}

void SeerVariableLoggerBrowserWidget::handleText (const QString& text) {

    QApplication::setOverrideCursor(Qt::BusyCursor);

    if (text.contains(QRegularExpression("^([0-9]+)\\^done,value="))) {

        //qDebug() << text;

        // "6^done,value=\"\\\"abc\\\"\""

        QString id_text    = text.section('^', 0,0);
        QString value_text = Seer::parseFirst(text, "value=", '"', '"', false);

        if (_ids.contains(id_text.toInt()) == true) {

            QList<QTreeWidgetItem*> matches = variablesTreeWidget->findItems(id_text, Qt::MatchExactly, 0);

            if (matches.size() > 0) {
                matches.first()->setText(3, Seer::filterEscapes(value_text));
            }
        }

    }else if (text.contains(QRegularExpression("^([0-9]+)\\^error,msg="))) {

        //qDebug() << text;

        // "1^error,msg=\"No symbol \\\"j\\\" in current context.\""

        QString id_text  = text.section('^', 0,0);
        QString msg_text = Seer::parseFirst(text, "msg=", '"', '"', false);

        if (_ids.contains(id_text.toInt()) == true) {

            QList<QTreeWidgetItem*> matches = variablesTreeWidget->findItems(id_text, Qt::MatchExactly, 0);

            if (matches.size() > 0) {
                matches.first()->setText(2, ""); // Overwrite "name" with "" because it's not a valid "name".
                matches.first()->setText(3, Seer::filterEscapes(msg_text));
            }
        }

    }else if (text.startsWith("^error,msg=\"No registers.\"")) {

        variablesTreeWidget->clear();

    }else{
        // Ignore others.
    }

    // Resize columns.
    variablesTreeWidget->resizeColumnToContents(0);
    variablesTreeWidget->resizeColumnToContents(1);
    variablesTreeWidget->resizeColumnToContents(2);
    variablesTreeWidget->resizeColumnToContents(3);

    // Scroll to the bottom.
    QTreeWidgetItem* lastItem = variablesTreeWidget->topLevelItem(variablesTreeWidget->topLevelItemCount()-1);
    if (lastItem) {
        variablesTreeWidget->scrollToItem(lastItem);
    }

    // Set the cursor back.
    QApplication::restoreOverrideCursor();
}

void SeerVariableLoggerBrowserWidget::handleEvaluateVariableExpression (int expressionid, QString expression) {

    QString id_text = QString::number(expressionid);

    if (_ids.contains(id_text.toInt()) == false) {
        return;
    }

    QList<QTreeWidgetItem*> matches = variablesTreeWidget->findItems(id_text, Qt::MatchExactly, 0);

    // Reuse existing item.
    if (matches.size() > 0) {
        matches.first()->setText(1, QTime::currentTime().toString(Qt::TextDate));
        matches.first()->setText(2, "");
        matches.first()->setText(3, "");

    // Add new item.
    }else{
        /*
        QTreeWidgetItem* item = new QTreeWidgetItem;
        item->setText(0, id_text);
        item->setText(1, QTime::currentTime().toString(Qt::TextDate));
        item->setText(2, expression);
        item->setText(3, "");

        item->setFont(3, QFontDatabase::systemFont(QFontDatabase::FixedFont));

        variablesTreeWidget->addTopLevelItem(item);
        */

        // Populate the tree.
        handleItemCreate(0, timestamp_text, name_text, value_text);
    }

    // Resize columns done later in handleText().
}

void SeerVariableLoggerBrowserWidget::addVariableExpression (QString expression) {

    //qDebug();

    if (expression != "") {

        int id = Seer::createID();

        _ids.insert(id); // Keep track of which ones are entered.

        emit evaluateVariableExpression(id, expression);
    }
}

void SeerVariableLoggerBrowserWidget::handleAddLineEdit () {

    //qDebug();

    QString variable = variableAddLineEdit->text();

    variableAddLineEdit->clear();

    if (variable != "") {

        int id = Seer::createID();

        _ids.insert(id); // Keep track of which ones are entered.

        emit evaluateVariableExpression(id, variable);
    }
}

void SeerVariableLoggerBrowserWidget::handleDeleteToolButton () {

    // Get selected tree items.
    QList<QTreeWidgetItem*> items = variablesTreeWidget->selectedItems();

    // Remove them.
    for(int i=0; i<items.size(); i++){
        delete items[i];
    }

    // Resize columns.
    variablesTreeWidget->resizeColumnToContents(0);
    variablesTreeWidget->resizeColumnToContents(1);
    variablesTreeWidget->resizeColumnToContents(2);
    variablesTreeWidget->resizeColumnToContents(3);
}

void SeerVariableLoggerBrowserWidget::handleDeleteAllToolButton () {

    // Remove all.
    variablesTreeWidget->clear();

    // Resize columns.
    variablesTreeWidget->resizeColumnToContents(0);
    variablesTreeWidget->resizeColumnToContents(1);
    variablesTreeWidget->resizeColumnToContents(2);
    variablesTreeWidget->resizeColumnToContents(3);
}

void SeerVariableLoggerBrowserWidget::handleItemEntered (QTreeWidgetItem* item, int column) {

    Q_UNUSED(column);

    //qDebug() << item->text(0) << column;

    item->setToolTip(0, item->text(1) + " : " + item->text(2) + " : " + item->text(3));

    for (int i=1; i<variablesTreeWidget->columnCount(); i++) { // Copy tooltip to other columns.
        item->setToolTip(i, item->toolTip(0));
    }
}

void SeerVariableLoggerBrowserWidget::handleItemCreate (QTreeWidgetItem* parentItem, const QString& timestamp_text, const QString& name_text, const QString& value_text) {

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

    // Add the complex entry to the tree. Reuse, if possible.
    if (Seer::hasBookends(value_text, '{', '}')) {

        // Remove bookends
        QString text = Seer::filterBookends(value_text, '{', '}');

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
        qDeleteAll(children);

        // Populate the item.
        item->setText(0, name_text);
        item->setText(1, arg_text);
        item->setText(2, Seer::filterEscapes(value_text));
        item->setFont(2, QFontDatabase::systemFont(QFontDatabase::FixedFont));
    }
}

void SeerVariableLoggerBrowserWidget::handleContextMenu (const QPoint& pos) {

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

        text += items[i]->text(2) + ":" + items[i]->text(3);
    }

    clipboard->setText(text, QClipboard::Clipboard);
    clipboard->setText(text, QClipboard::Selection);
}

