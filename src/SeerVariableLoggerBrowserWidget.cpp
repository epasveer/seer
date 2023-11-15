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
    variablesTreeWidget->resizeColumnToContents(0); // timestamp
    variablesTreeWidget->resizeColumnToContents(1); // name
    variablesTreeWidget->resizeColumnToContents(2); // value
    variablesTreeWidget->resizeColumnToContents(3); // id
    variablesTreeWidget->setColumnHidden(3, true);  // Hide the 'id' column.
    variablesTreeWidget->clear();

    // Connect things.
    QObject::connect(this,                           &SeerVariableLoggerBrowserWidget::evaluateVariableExpression,      this, &SeerVariableLoggerBrowserWidget::handleEvaluateVariableExpression);
    QObject::connect(variableAddLineEdit,            &QLineEdit::returnPressed,                                         this, &SeerVariableLoggerBrowserWidget::handleAddLineEdit);
    QObject::connect(variableDeleteToolButton,       &QToolButton::clicked,                                             this, &SeerVariableLoggerBrowserWidget::handleDeleteToolButton);
    QObject::connect(variableDeleteAllToolButton,    &QToolButton::clicked,                                             this, &SeerVariableLoggerBrowserWidget::handleDeleteAllToolButton);
    QObject::connect(variablesTreeWidget,            &QTreeWidget::itemCollapsed,                                       this, &SeerVariableLoggerBrowserWidget::handleItemCollapsed);
    QObject::connect(variablesTreeWidget,            &QTreeWidget::itemExpanded,                                        this, &SeerVariableLoggerBrowserWidget::handleItemExpanded);
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

        if (_ids.contains(id_text.toInt()) == false) {
            QApplication::restoreOverrideCursor();
            return;
        }

        QList<QTreeWidgetItem*> matches = variablesTreeWidget->findItems(id_text, Qt::MatchExactly, 3);

        if (matches.size() > 0) {

            QTreeWidgetItem* match = matches[0];

            Q_ASSERT(match->parent() == NULL);

            QString timestamp_text = match->text(0);
            QString name_text      = match->text(1);

            // Populate the tree.
            handleItemCreate(match, id_text, timestamp_text, name_text, value_text);
        }

    }else if (text.contains(QRegularExpression("^([0-9]+)\\^error,msg="))) {

        //qDebug() << text;

        // "1^error,msg=\"No symbol \\\"j\\\" in current context.\""

        QString id_text  = text.section('^', 0,0);
        QString msg_text = Seer::parseFirst(text, "msg=", '"', '"', false);

        if (_ids.contains(id_text.toInt()) == true) {

            QList<QTreeWidgetItem*> matches = variablesTreeWidget->findItems(id_text, Qt::MatchExactly, 3);

            if (matches.size() > 0) {
                matches.first()->setText(1, ""); // Overwrite "name" with "" because it's not a valid "name".
                matches.first()->setText(2, Seer::filterEscapes(msg_text));
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

    // Add new item. Will be filled in by handleText().
    QTreeWidgetItem* item = new QTreeWidgetItem;
    item->setText(0, QTime::currentTime().toString(Qt::TextDate));
    item->setText(1, expression);
    item->setText(2, "");
    item->setText(3, id_text);

    item->setFont(2, QFontDatabase::systemFont(QFontDatabase::FixedFont));

    variablesTreeWidget->addTopLevelItem(item);

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

    QString expression = variableAddLineEdit->text();

    variableAddLineEdit->clear();

    if (expression != "") {

        int id = Seer::createID();

        _ids.insert(id); // Keep track of which ones are entered.

        emit evaluateVariableExpression(id, expression);
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

void SeerVariableLoggerBrowserWidget::handleItemExpanded (QTreeWidgetItem* item) {

    Q_UNUSED(item);

    variablesTreeWidget->resizeColumnToContents(0);
    variablesTreeWidget->resizeColumnToContents(1);
    variablesTreeWidget->resizeColumnToContents(2);
    variablesTreeWidget->resizeColumnToContents(3);
}

void SeerVariableLoggerBrowserWidget::handleItemCollapsed (QTreeWidgetItem* item) {

    Q_UNUSED(item);

    variablesTreeWidget->resizeColumnToContents(0);
    variablesTreeWidget->resizeColumnToContents(1);
    variablesTreeWidget->resizeColumnToContents(2);
    variablesTreeWidget->resizeColumnToContents(3);
}

void SeerVariableLoggerBrowserWidget::handleItemEntered (QTreeWidgetItem* item, int column) {

    Q_UNUSED(column);

    //qDebug() << item->text(3) << column;

    item->setToolTip(0, item->text(0) + " : " + item->text(1) + " : " + item->text(2));

    for (int i=1; i<variablesTreeWidget->columnCount(); i++) { // Copy tooltip to other columns.
        item->setToolTip(i, item->toolTip(0));
    }
}

void SeerVariableLoggerBrowserWidget::handleItemCreate (QTreeWidgetItem* parentItem, const QString& id_text, const QString& timestamp_text, const QString& name_text, const QString& value_text) {

    // Add the complex entry to the tree.
    if (Seer::hasBookends(value_text, '{', '}')) {

        // Remove bookends
        QString text = Seer::filterBookends(value_text, '{', '}');

        // Set the flatvalue text.
        parentItem->setText(0, timestamp_text);
        parentItem->setText(1, name_text);
        parentItem->setText(2, Seer::filterEscapes(text));
        parentItem->setFont(2, QFontDatabase::systemFont(QFontDatabase::FixedFont));
        parentItem->setText(3, id_text);

        // Convert to a list of name/value pairs.
        QStringList nv_pairs = Seer::parseCommaList(text, '{', '}');

        // Go through each pair and add the name and its value to the tree.
        for (const auto& nv : nv_pairs) {

            QStringPair pair = Seer::parseNameValue(nv, '=');

            // Create a new item and attach it to the parent.
            QTreeWidgetItem* item = new QTreeWidgetItem;

            handleItemCreate(item, id_text, timestamp_text, pair.first, pair.second);

            parentItem->addChild(item);
        }

    // Add the simple entry to the tree.
    }else{
        parentItem->setText(0, timestamp_text);
        parentItem->setText(1, name_text);
        parentItem->setText(2, Seer::filterEscapes(value_text));
        parentItem->setFont(2, QFontDatabase::systemFont(QFontDatabase::FixedFont));
        parentItem->setText(3, id_text);
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

        text += items[i]->text(1) + ":" + items[i]->text(2);
    }

    clipboard->setText(text, QClipboard::Clipboard);
    clipboard->setText(text, QClipboard::Selection);
}

