#include "SeerStackDumpBrowserWidget.h"
#include "SeerUtl.h"
#include <QtWidgets/QTableWidget>
#include <QtWidgets/QApplication>
#include <QtCore/QDebug>

SeerStackDumpBrowserWidget::SeerStackDumpBrowserWidget (QWidget* parent) : QWidget(parent) {

    _spExpressionId = Seer::createID();

    // Construct the UI.
    setupUi(this);

    // Setup the widgets
    stackTableWidget->setMouseTracking(true);
    stackTableWidget->setSortingEnabled(false);
    stackTableWidget->resizeColumnToContents(0); // 2 byte address
    stackTableWidget->resizeColumnToContents(1); // 2 byte value
    stackTableWidget->resizeColumnToContents(2); // 4 byte value
    stackTableWidget->resizeColumnToContents(3); // 8 byte value
    stackTableWidget->resizeColumnToContents(4); // 8 byte ascii

    stackTableWidget->clearContents();

    // Connect things.
}

SeerStackDumpBrowserWidget::~SeerStackDumpBrowserWidget () {
}

void SeerStackDumpBrowserWidget::handleText (const QString& text) {

    // -data-read-memory-bytes -o 16 $sp 32
    // ^done,memory=[
    //               {
    //                  begin="0x00007fffffffda80",
    //                  offset="0x0000000000000000",
    //                  end="0x00007fffffffdaa0",
    //                  contents="0000000000000000e80300000000000098dcffffff7f0000e803000001000000"
    //               }
    //             ]
    //
    //
    // -data-evaluate-expression $sp
    // ^done,value="0x7fffffffda90"


    QApplication::setOverrideCursor(Qt::BusyCursor);

    while (1) {
        if (text.contains(QRegularExpression("^([0-9]+)\\^done,value="))) {

            // ^done,value="0x7fffffffda90"
            QString id_text    = text.section('^', 0,0);
            QString value_text = Seer::parseFirst(text, "value=", '"', '"', false);

            if (id_text.toInt() != _spExpressionId) {
                break;
            }

            addressLineEdit->setText(value_text);

        }else if (text.contains(QRegularExpression("^([0-9]+)\\^error,msg=\"No registers.\""))) {

            addressLineEdit->setText("");

        }else if (text.startsWith("^done,memory=[") && text.endsWith("]")) {

        }else if (text.startsWith("^error,msg=\"No registers.\"")) {
            stackTableWidget->clearContents();

        }else{
            // Ignore others.
        }
        break;
    }

    stackTableWidget->resizeColumnToContents(0);
    stackTableWidget->resizeColumnToContents(1);
    stackTableWidget->resizeColumnToContents(2);
    stackTableWidget->resizeColumnToContents(3);
    stackTableWidget->resizeColumnToContents(4);

    QApplication::restoreOverrideCursor();
}

void SeerStackDumpBrowserWidget::handleStoppingPointReached () {

    // Don't do any work if the widget is hidden.
    if (isHidden()) {
        return;
    }

    refresh();
}

void SeerStackDumpBrowserWidget::refresh () {

    // Don't do any work if the widget is hidden.
    if (isHidden()) {
        return;
    }

    emit refreshStackPointer(_spExpressionId, "$sp");
}

void SeerStackDumpBrowserWidget::showEvent (QShowEvent* event) {

    QWidget::showEvent(event);

    refresh();
}

