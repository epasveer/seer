#include "SeerVariableLoggerBrowserWidget.h"
#include "SeerUtl.h"
#include <QtWidgets/QTreeWidget>
#include <QtWidgets/QTreeWidgetItemIterator>
#include <QtWidgets/QApplication>
#include <QtCore/QRegExp>
#include <QtCore/QTime>
#include <QtCore/QDebug>
#include <iostream>

SeerVariableLoggerBrowserWidget::SeerVariableLoggerBrowserWidget (QWidget* parent) : QWidget(parent) {

    // Construct the UI.
    setupUi(this);

    // Setup the widgets
    variablesTreeWidget->setMouseTracking(true);
    variablesTreeWidget->setSortingEnabled(false);
    variablesTreeWidget->setSelectionMode(QAbstractItemView::ExtendedSelection);
    variablesTreeWidget->resizeColumnToContents(0); // id
    variablesTreeWidget->resizeColumnToContents(1); // timestamp
    variablesTreeWidget->resizeColumnToContents(2); // name
    variablesTreeWidget->resizeColumnToContents(3); // value

    variablesTreeWidget->setColumnHidden(0, true); // Hide the 'number' column.

    variablesTreeWidget->clear();

    // Connect things.
    QObject::connect(this,                           &SeerVariableLoggerBrowserWidget::evaluateVariableExpression,      this, &SeerVariableLoggerBrowserWidget::handleEvaluateVariableExpression);
    QObject::connect(variableAddLineEdit,            &QLineEdit::returnPressed,                                         this, &SeerVariableLoggerBrowserWidget::handleAddLineEdit);
    QObject::connect(variableDeleteToolButton,       &QToolButton::clicked,                                             this, &SeerVariableLoggerBrowserWidget::handleDeleteToolButton);
    QObject::connect(variableDeleteAllToolButton,    &QToolButton::clicked,                                             this, &SeerVariableLoggerBrowserWidget::handleDeleteAllToolButton);
    QObject::connect(variablesTreeWidget,            &QTreeWidget::itemEntered,                                         this, &SeerVariableLoggerBrowserWidget::handleItemEntered);
}

SeerVariableLoggerBrowserWidget::~SeerVariableLoggerBrowserWidget () {
}

void SeerVariableLoggerBrowserWidget::handleText (const QString& text) {

    QApplication::setOverrideCursor(Qt::BusyCursor);

    if (text.contains(QRegExp("^([0-9]+)\\^done,value="))) {

        //qDebug() << text;

        // "6^done,value=\"\\\"abc\\\"\""

        QString id_text    = text.section('^', 0,0);
        QString value_text = Seer::parseFirst(text, "value=", '"', '"', false);

        QList<QTreeWidgetItem*> matches = variablesTreeWidget->findItems(id_text, Qt::MatchExactly, 0);

        if (matches.size() > 0) {
            matches.first()->setText(3, Seer::filterEscapes(value_text));
        }

    }else if (text.contains(QRegExp("^([0-9]+)\\^error,msg="))) {

        //qDebug() << text;

        // "1^error,msg=\"No symbol \\\"j\\\" in current context.\""

        QString id_text  = text.section('^', 0,0);
        QString msg_text = Seer::parseFirst(text, "msg=", '"', '"', false);

        QList<QTreeWidgetItem*> matches = variablesTreeWidget->findItems(id_text, Qt::MatchExactly, 0);

        if (matches.size() > 0) {
            matches.first()->setText(3, Seer::filterEscapes(msg_text));
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

    QList<QTreeWidgetItem*> matches = variablesTreeWidget->findItems(id_text, Qt::MatchExactly, 0);

    // Reuse existing item.
    if (matches.size() > 0) {
        matches.first()->setText(1, QTime::currentTime().toString(Qt::TextDate));
        matches.first()->setText(2, "");
        matches.first()->setText(3, "");

    // Add new item.
    }else{
        QTreeWidgetItem* item = new QTreeWidgetItem;
        item->setText(0, id_text);
        item->setText(1, QTime::currentTime().toString(Qt::TextDate));
        item->setText(2, expression);
        item->setText(3, "");

        variablesTreeWidget->addTopLevelItem(item);
    }

    // Resize columns.
    variablesTreeWidget->resizeColumnToContents(0);
    variablesTreeWidget->resizeColumnToContents(1);
    variablesTreeWidget->resizeColumnToContents(2);
    variablesTreeWidget->resizeColumnToContents(3);
}

void SeerVariableLoggerBrowserWidget::addVariableExpression (QString expression) {

    //qDebug();

    if (expression != "") {

        int id = Seer::createID();

        emit evaluateVariableExpression(id, expression);
    }
}

void SeerVariableLoggerBrowserWidget::handleAddLineEdit () {

    //qDebug();

    QString variable = variableAddLineEdit->text();

    variableAddLineEdit->clear();

    if (variable != "") {

        int id = Seer::createID();

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

