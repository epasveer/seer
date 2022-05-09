#include "SeerVariableBrowserWidget.h"
#include "SeerUtl.h"
#include <QtWidgets/QTreeWidget>
#include <QtWidgets/QTreeWidgetItemIterator>
#include <QtWidgets/QLabel>
#include <QtWidgets/QApplication>
#include <QtCore/QFileInfo>
#include <QtCore/Qt>
#include <QtCore/QMap>
#include <QtCore/QDebug>

SeerVariableBrowserWidget::SeerVariableBrowserWidget (QWidget* parent) : QWidget(parent) {

    // Set the state.
    _id = Seer::createID();

    // Construct the UI.
    setupUi(this);

    // Setup the widgets
    variableNameSearchLineEdit->setPlaceholderText("Variable Name...");
    variableNameSearchLineEdit->setClearButtonEnabled(true);
    variableTypeSearchLineEdit->setPlaceholderText("Variable Type...");
    variableTypeSearchLineEdit->setClearButtonEnabled(true);
    variableTreeWidget->setMouseTracking(true);
  //variableTreeWidget->resizeColumnToContents(0);
  //variableTreeWidget->resizeColumnToContents(1);
    variableTreeWidget->resizeColumnToContents(2);
    variableTreeWidget->resizeColumnToContents(3);
    variableTreeWidget->resizeColumnToContents(4);
    variableTreeWidget->resizeColumnToContents(5);
    variableTreeWidget->clear();
    variableTreeWidget->setSortingEnabled(false);

    // Connect things.
    QObject::connect(variableTreeWidget,          &QTreeWidget::itemDoubleClicked,    this,  &SeerVariableBrowserWidget::handleItemDoubleClicked);
    QObject::connect(variableTreeWidget,          &QTreeWidget::itemEntered,          this,  &SeerVariableBrowserWidget::handleItemEntered);
    QObject::connect(variableNameSearchLineEdit,  &QLineEdit::returnPressed,          this,  &SeerVariableBrowserWidget::handleSearchLineEdit);
    QObject::connect(variableTypeSearchLineEdit,  &QLineEdit::returnPressed,          this,  &SeerVariableBrowserWidget::handleSearchLineEdit);
}

SeerVariableBrowserWidget::~SeerVariableBrowserWidget () {
}

void SeerVariableBrowserWidget::handleText (const QString& text) {

    QApplication::setOverrideCursor(Qt::BusyCursor);

    if (text.startsWith(QString::number(_id) + "^done,symbols={") && text.endsWith("}")) {

        variableTreeWidget->clear();
        variableTreeWidget->setSortingEnabled(false);
        variableTreeWidget->sortByColumn(-1, Qt::AscendingOrder);

        // -symbol-info-variables
        // ^done,symbols={
        //          debug=[
        //                  {
        //                      filename="elf-init.c",
        //                      fullname="/home/abuild/rpmbuild/BUILD/glibc-2.31/csu/elf-init.c",
        //                      symbols=[
        //                                  {
        //                                      line="95",
        //                                      name="__libc_csu_fini",
        //                                      type="void (void)",
        //                                      description="void __libc_csu_fini(void);"
        //                                  },
        //                                  {
        //                                      line="67",
        //                                      name="__libc_csu_init",
        //                                      type="void (int, char **, char **)",
        //                                      description="void __libc_csu_init(int, char **, char **);"
        //                                  }
        //                              ]
        //                  },
        //                  ...
        //                ]
        //              }

        QString debug_text = Seer::parseFirst(text, "debug=", '[', ']', false);

        QStringList filenames_list = Seer::parse(debug_text, "", '{', '}', false);

        for (const auto& filename_entry : filenames_list) {

            QString filename_text = Seer::parseFirst(filename_entry, "filename=", '"', '"', false);
            QString fullname_text = Seer::parseFirst(filename_entry, "fullname=", '"', '"', false);

            QString symbols_text = Seer::parseFirst(filename_entry, "symbols=", '[', ']', false);

            QStringList symbols_list = Seer::parse(symbols_text, "", '{', '}', false);

            for (const auto& symbol_entry : symbols_list) {

                QString line_text        = Seer::parseFirst(symbol_entry, "line=", '"', '"', false);
                QString name_text        = Seer::parseFirst(symbol_entry, "name=", '"', '"', false);
                QString type_text        = Seer::parseFirst(symbol_entry, "type=", '"', '"', false);
                QString description_text = Seer::parseFirst(symbol_entry, "type=", '"', '"', false);

                // Skip variable entries that have no line number.
                if (line_text == "") {
                    continue;
                }

                // Add the variable to the tree.
                QTreeWidgetItem* item = new QTreeWidgetItem;

                QFont f0 = item->font(0);
                f0.setBold(true);
                item->setFont(0,f0);
                item->setFont(1,f0);

                item->setText(0, name_text);
                item->setText(1, type_text);
                item->setText(2, filename_text);
                item->setText(3, line_text);
                item->setText(4, fullname_text);
                item->setText(5, description_text);

                variableTreeWidget->addTopLevelItem(item);
            }
        }

    }else{
        // Ignore others.
    }

  //variableTreeWidget->resizeColumnToContents(0);  // Name
  //variableTreeWidget->resizeColumnToContents(1);  // Type
    variableTreeWidget->resizeColumnToContents(2);  // Filename
    variableTreeWidget->resizeColumnToContents(3);  // Line
    variableTreeWidget->resizeColumnToContents(4);  // Fullname
    variableTreeWidget->resizeColumnToContents(5);  // Description
    variableTreeWidget->sortByColumn(0, Qt::AscendingOrder);
    variableTreeWidget->setSortingEnabled(true);

    QApplication::restoreOverrideCursor();
}

void SeerVariableBrowserWidget::handleItemDoubleClicked (QTreeWidgetItem* item, int column) {

    Q_UNUSED(column);

    emit selectedFile(item->text(2), item->text(4), item->text(3).toInt());
}

void SeerVariableBrowserWidget::handleItemEntered (QTreeWidgetItem* item, int column) {

    Q_UNUSED(column);

    QString tip = QString("Variable: %1\nType: %2\nFile: %3\nLine: %4\nFullname: %5\nDescription: %6").arg(item->text(0)).arg(item->text(1)).arg(item->text(2)).arg(item->text(3)).arg(item->text(4)).arg(item->text(5));

    item->setToolTip(0, tip);

    for (int i=1; i<variableTreeWidget->columnCount(); i++) { // Copy tooltip to the other columns.
        item->setToolTip(i, item->toolTip(0));
    }
}

void SeerVariableBrowserWidget::handleSearchLineEdit () {

    variableTreeWidget->clear();
    variableTreeWidget->setSortingEnabled(false);
    variableTreeWidget->sortByColumn(-1, Qt::AscendingOrder);

  //variableTreeWidget->resizeColumnToContents(0);
  //variableTreeWidget->resizeColumnToContents(1);
    variableTreeWidget->resizeColumnToContents(2);
    variableTreeWidget->resizeColumnToContents(3);
    variableTreeWidget->resizeColumnToContents(4);
    variableTreeWidget->resizeColumnToContents(5);

    if (variableNameSearchLineEdit->text() != "" || variableTypeSearchLineEdit->text() != "") {
        emit refreshVariableList(_id, variableNameSearchLineEdit->text(), variableTypeSearchLineEdit->text());
    }
}

void SeerVariableBrowserWidget::refresh () {
    handleSearchLineEdit();
}

