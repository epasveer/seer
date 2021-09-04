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
    argumentsTreeWidget->setSortingEnabled(false);
    argumentsTreeWidget->resizeColumnToContents(0); // level
    argumentsTreeWidget->resizeColumnToContents(1); // Name
    argumentsTreeWidget->resizeColumnToContents(2); // Value

    argumentsTreeWidget->clear();

    // Connect things.
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

        //qDebug() << __PRETTY_FUNCTION__ << ":" << newtext;

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

        //qDebug() << __PRETTY_FUNCTION__ << ":" << frame_list;

        for ( const auto& frame_text : frame_list  ) {

            //qDebug() << __PRETTY_FUNCTION__ << ":" << frame_text;

            QString level_text = Seer::parseFirst(frame_text, "level=", '"', '"', false);
            QString args_text  = Seer::parseFirst(frame_text, "args=",  '[', ']', false);

            //qDebug() << __PRETTY_FUNCTION__ << ":" << level_text;
            //qDebug() << __PRETTY_FUNCTION__ << ":" << args_text;

            if (level_text == "" || args_text == "") {
                break;
            }

            QStringList namevalue_list  = Seer::parse(args_text, "",  '{', '}', false);

            if (namevalue_list.size() == 0) {
                break;
            }

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

void SeerStackArgumentsBrowserWidget::showEvent (QShowEvent* event) {

    QWidget::showEvent(event);

    refresh();
}

