#include "SeerStackArgumentsBrowserWidget.h"
#include "SeerUtl.h"
#include <QtWidgets/QTreeWidget>
#include <QtWidgets/QTreeWidgetItemIterator>
#include <QtWidgets/QApplication>
#include <QtCore/QDebug>

SeerStackArgumentsBrowserWidget::SeerStackArgumentsBrowserWidget (QWidget* parent) : QWidget(parent) {

    // Construct the UI.
    setupUi(this);

    // Setup the widgets
    argumentsTreeWidget->setMouseTracking(true);
    argumentsTreeWidget->setSortingEnabled(false);
    argumentsTreeWidget->resizeColumnToContents(0); // level
    argumentsTreeWidget->resizeColumnToContents(1); // Name
    argumentsTreeWidget->resizeColumnToContents(2); // Value

    argumentsTreeWidget->clear();

    // Connect things.
    QObject::connect(argumentsTreeWidget, &QTreeWidget::itemEntered,      this,  &SeerStackArgumentsBrowserWidget::handleItemEntered);
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

