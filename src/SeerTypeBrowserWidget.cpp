#include "SeerTypeBrowserWidget.h"
#include "SeerUtl.h"
#include <QtWidgets/QTreeWidget>
#include <QtWidgets/QTreeWidgetItemIterator>
#include <QtWidgets/QLabel>
#include <QtWidgets/QApplication>
#include <QtCore/QFileInfo>
#include <QtCore/Qt>
#include <QtCore/QMap>
#include <QtCore/QDebug>

SeerTypeBrowserWidget::SeerTypeBrowserWidget (QWidget* parent) : QWidget(parent) {

    // Set the state.
    _id = Seer::createID();

    // Construct the UI.
    setupUi(this);

    // Setup the widgets
    typeSearchLineEdit->setPlaceholderText("Search...");
    typeSearchLineEdit->setClearButtonEnabled(true);
    typeTreeWidget->setMouseTracking(true);
  //typeTreeWidget->resizeColumnToContents(0);
    typeTreeWidget->resizeColumnToContents(1);
    typeTreeWidget->resizeColumnToContents(2);
    typeTreeWidget->resizeColumnToContents(3);
    typeTreeWidget->clear();
    typeTreeWidget->setSortingEnabled(false);

    // Connect things.
    QObject::connect(typeTreeWidget,      &QTreeWidget::itemDoubleClicked,    this,  &SeerTypeBrowserWidget::handleItemDoubleClicked);
    QObject::connect(typeTreeWidget,      &QTreeWidget::itemEntered,          this,  &SeerTypeBrowserWidget::handleItemEntered);
    QObject::connect(typeSearchLineEdit,  &QLineEdit::returnPressed,          this,  &SeerTypeBrowserWidget::handleSearchLineEdit);
}

SeerTypeBrowserWidget::~SeerTypeBrowserWidget () {
}

void SeerTypeBrowserWidget::handleText (const QString& text) {

    QApplication::setOverrideCursor(Qt::BusyCursor);

    if (text.startsWith(QString::number(_id) + "^done,symbols={") && text.endsWith("}")) {

        typeTreeWidget->clear();
        typeTreeWidget->setSortingEnabled(false);
        typeTreeWidget->sortByColumn(-1, Qt::AscendingOrder);

        // -symbol-info-types [--name name_regexp]
        //                    [--max-results limit]
        //
        // ^done,symbols={
        //          debug=[
        //                  {
        //                      filename="/peak/rel/include/CnvLib.h",fullname="/home/erniep/Development/Peak/rel/include/CnvLib.h",
        //                      symbols=[
        //                                  {
        //                                      line="8",
        //                                      name="Cnv"
        //                                  }
        //                              ]
        //                  },
        //                  ...
        //                 ]
        //               }

        QString debug_text = Seer::parseFirst(text, "debug=", '[', ']', false);

        QStringList filenames_list = Seer::parse(debug_text, "", '{', '}', false);

        for (const auto& filename_entry : filenames_list) {

            QString filename_text = Seer::parseFirst(filename_entry, "filename=", '"', '"', false);
            QString fullname_text = Seer::parseFirst(filename_entry, "fullname=", '"', '"', false);

            QString symbols_text = Seer::parseFirst(filename_entry, "symbols=", '[', ']', false);

            QStringList symbols_list = Seer::parse(symbols_text, "", '{', '}', false);

            for (const auto& symbol_entry : symbols_list) {

                QString line_text = Seer::parseFirst(symbol_entry, "line=", '"', '"', false);
                QString name_text = Seer::parseFirst(symbol_entry, "name=", '"', '"', false);

                // Skip type entries that have no line number.
                if (line_text == "") {
                    continue;
                }

                // Add the type to the tree.
                QTreeWidgetItem* item = new QTreeWidgetItem;

                QFont f0 = item->font(0);
                f0.setBold(true);
                item->setFont(0,f0);

                item->setText(0, name_text);
                item->setText(1, filename_text);
                item->setText(2, line_text);
                item->setText(3, fullname_text);

                typeTreeWidget->addTopLevelItem(item);
            }
        }

    }else{
        // Ignore others.
    }

  //typeTreeWidget->resizeColumnToContents(0);
    typeTreeWidget->resizeColumnToContents(1);
    typeTreeWidget->resizeColumnToContents(2);
    typeTreeWidget->resizeColumnToContents(3);
    typeTreeWidget->sortByColumn(0, Qt::AscendingOrder);
    typeTreeWidget->setSortingEnabled(true);

    QApplication::restoreOverrideCursor();
}

void SeerTypeBrowserWidget::handleItemDoubleClicked (QTreeWidgetItem* item, int column) {

    Q_UNUSED(column);

    emit selectedFile(item->text(1), item->text(3), item->text(2).toInt());
}

void SeerTypeBrowserWidget::handleItemEntered (QTreeWidgetItem* item, int column) {

    Q_UNUSED(column);

    QString tip = QString("Type: %1\nFile: %2\nLine: %3\nFullname: %4").arg(item->text(0)).arg(item->text(1)).arg(item->text(2)).arg(item->text(3));

    item->setToolTip(0, tip);

    for (int i=1; i<typeTreeWidget->columnCount(); i++) { // Copy tooltip to the other columns.
        item->setToolTip(i, item->toolTip(0));
    }
}

void SeerTypeBrowserWidget::handleSearchLineEdit () {

    typeTreeWidget->clear();
    typeTreeWidget->setSortingEnabled(false);
    typeTreeWidget->sortByColumn(-1, Qt::AscendingOrder);

  //typeTreeWidget->resizeColumnToContents(0);
    typeTreeWidget->resizeColumnToContents(1);
    typeTreeWidget->resizeColumnToContents(2);
    typeTreeWidget->resizeColumnToContents(3);

    if (typeSearchLineEdit->text() != "") {
        emit refreshTypeList(_id, typeSearchLineEdit->text());
    }
}

void SeerTypeBrowserWidget::refresh () {
    handleSearchLineEdit();
}

