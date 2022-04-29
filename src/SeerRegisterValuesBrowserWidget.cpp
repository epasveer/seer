#include "SeerRegisterValuesBrowserWidget.h"
#include "SeerUtl.h"
#include <QtWidgets/QTreeWidget>
#include <QtWidgets/QTreeWidgetItemIterator>
#include <QtWidgets/QApplication>
#include <QtCore/QDebug>

SeerRegisterValuesBrowserWidget::SeerRegisterValuesBrowserWidget (QWidget* parent) : QWidget(parent) {

    // Construct the UI.
    setupUi(this);

    // Setup the widgets
    registersTreeWidget->setMouseTracking(true);
    registersTreeWidget->setSortingEnabled(false);
    registersTreeWidget->resizeColumnToContents(0); // number
    registersTreeWidget->resizeColumnToContents(1); // name
    registersTreeWidget->resizeColumnToContents(2); // value
    registersTreeWidget->resizeColumnToContents(3); // used

    registersTreeWidget->setColumnHidden(0, true); // Hide the 'number' column.
    registersTreeWidget->setColumnHidden(3, true); // Hide the 'used' column.

    registersTreeWidget->clear();

    // Connect things.
    QObject::connect(registersTreeWidget, &QTreeWidget::itemEntered,          this, &SeerRegisterValuesBrowserWidget::handleItemEntered);
}

SeerRegisterValuesBrowserWidget::~SeerRegisterValuesBrowserWidget () {
}

void SeerRegisterValuesBrowserWidget::handleText (const QString& text) {

    // Don't do any work if the widget is hidden.
    if (isHidden()) {
        return;
    }

    QApplication::setOverrideCursor(Qt::BusyCursor);

    if (text.startsWith("^done,register-names=[") && text.endsWith("]")) {

        // "^done,register-names=[\"rax\",\"rbx\",\"rcx\",\"rdx\",\"rsi\",\"rdi\",\"rbp\",\"rsp\",
        //                        \"r8\",\"r9\",\"r10\",\"r11\",\"r12\",\"r13\",\"r14\",\"r15\",
        //                        \"rip\",\"eflags\",\"cs\",\"ss\",\"ds\",\"es\",\"fs\",\"gs\",

        // This recreates the tree.
        registersTreeWidget->clear();

        QString newtext = Seer::filterEscapes(text); // Filter escaped characters.

        QString frame_text = Seer::parseFirst(newtext, "register-names=", '[', ']', false);

        //qDebug() << frame_text;

        QStringList name_list = Seer::parse(frame_text, "", '"', '"', false);

        int i = 0;
        for ( const auto& name_text : name_list  ) {

            if (name_text == "") {
                continue;
            }

            QTreeWidgetItem* topItem = new QTreeWidgetItem;
            topItem->setText(0, QString::number(i));
            topItem->setText(1, name_text);
            topItem->setText(2, "");
            topItem->setText(3, "new");

            registersTreeWidget->addTopLevelItem(topItem);

            i++;
        }

    }else if (text.startsWith("^done,register-values=[") && text.endsWith("]")) {

        // Mark each entry initially as "unused".
        // Later, some will be marked as "reused" or "new". Then the "unused" ones will
        // be deleted.
        QTreeWidgetItemIterator it(registersTreeWidget);
        while (*it) {
            (*it)->setText(3, "unused");
            ++it;
        }

        // "^done,register-values=[{number=\"0\",value=\"0x4005e7\"},{number=\"1\",value=\"0x0\"},{number=\"2\",value=\"0x100\"},
        //                    {number=\"3\",value=\"0x7fffffffd548\"},{number=\"4\",value=\"0x7fffffffd538\"},{number=\"5\",value=\"0x1\"},...
        //                    {number=\"205\",value=\"0x0\"},{number=\"206\",value=\"0x0\"}]"

        //qDebug() << text;

        QString newtext = Seer::filterEscapes(text); // Filter escaped characters.

        QString frame_text = Seer::parseFirst(newtext, "register-values=", '[', ']', false);

        QStringList registers_list = Seer::parse(frame_text, "", '{', '}', false);

        for ( const auto& register_text : registers_list  ) {

            QString number_text = Seer::parseFirst(register_text, "number=",  '"', '"', false);
            QString value_text  = Seer::parseFirst(register_text, "value=",   '"', '"', false);

            // Instead of creating a new tree each time, we will reuse existing items, if they are there.
            // This allows the expanded items to remain expanded.
            QList<QTreeWidgetItem*> matches = registersTreeWidget->findItems(number_text, Qt::MatchExactly, 0);

            // Found a match. Reuse it. Set the value.
            // If no matches, do not add this entry.
            if (matches.size() > 0) {

                QTreeWidgetItem* item = matches.takeFirst();

                item->setText(2, value_text);
                item->setText(3, "reused");
            }
        }

        // At this point, there are some new entries, some reused entries, and some unused ones.
        // Delete the unused ones. They are obsolete.
        QList<QTreeWidgetItem*> matches = registersTreeWidget->findItems("unused", Qt::MatchExactly, 3);

        qDeleteAll(matches);

    }else if (text.startsWith("^error,msg=\"No registers.\"")) {
        registersTreeWidget->clear();

    }else{
        // Ignore others.
    }

    registersTreeWidget->resizeColumnToContents(0);
    registersTreeWidget->resizeColumnToContents(1);
    registersTreeWidget->resizeColumnToContents(2);
    registersTreeWidget->resizeColumnToContents(3);

    QApplication::restoreOverrideCursor();
}

void SeerRegisterValuesBrowserWidget::handleStoppingPointReached () {

    // Don't do any work if the widget is hidden.
    if (isHidden()) {
        return;
    }

    refresh();
}

void SeerRegisterValuesBrowserWidget::refresh () {
    emit refreshRegisterNames();
    emit refreshRegisterValues();
}

void SeerRegisterValuesBrowserWidget::handleItemEntered (QTreeWidgetItem* item, int column) {

    Q_UNUSED(column);

    //qDebug() << item->text(0) << column;

    item->setToolTip(0, item->text(1) + " : " + item->text(2));

    for (int i=1; i<registersTreeWidget->columnCount(); i++) { // Copy tooltip to other columns.
        item->setToolTip(i, item->toolTip(0));
    }
}

void SeerRegisterValuesBrowserWidget::showEvent (QShowEvent* event) {

    QWidget::showEvent(event);

    refresh();
}

