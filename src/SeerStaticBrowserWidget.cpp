#include "SeerStaticBrowserWidget.h"
#include "SeerUtl.h"
#include <QtWidgets/QTreeWidget>
#include <QtWidgets/QTreeWidgetItemIterator>
#include <QtWidgets/QLabel>
#include <QtWidgets/QApplication>
#include <QtCore/QFileInfo>
#include <QtCore/Qt>
#include <QtCore/QMap>
#include <QtCore/QDebug>

SeerStaticBrowserWidget::SeerStaticBrowserWidget (QWidget* parent) : QWidget(parent) {

    // Set the state.
    _id = Seer::createID();

    // Construct the UI.
    setupUi(this);

    // Setup the widgets
    staticNameSearchLineEdit->setPlaceholderText("Static name regex...");
    staticNameSearchLineEdit->setClearButtonEnabled(true);
    staticTypeSearchLineEdit->setPlaceholderText("Static type regex...");
    staticTypeSearchLineEdit->setClearButtonEnabled(true);
    staticTreeWidget->setMouseTracking(true);
  //staticTreeWidget->resizeColumnToContents(0);
  //staticTreeWidget->resizeColumnToContents(1);
    staticTreeWidget->resizeColumnToContents(2);
    staticTreeWidget->resizeColumnToContents(3);
    staticTreeWidget->resizeColumnToContents(4);
    staticTreeWidget->resizeColumnToContents(5);
    staticTreeWidget->clear();
    staticTreeWidget->setSortingEnabled(false);

    // Connect things.
    QObject::connect(staticTreeWidget,          &QTreeWidget::itemDoubleClicked,    this,  &SeerStaticBrowserWidget::handleItemDoubleClicked);
    QObject::connect(staticTreeWidget,          &QTreeWidget::itemEntered,          this,  &SeerStaticBrowserWidget::handleItemEntered);
    QObject::connect(staticNameSearchLineEdit,  &QLineEdit::returnPressed,          this,  &SeerStaticBrowserWidget::handleSearchLineEdit);
    QObject::connect(staticTypeSearchLineEdit,  &QLineEdit::returnPressed,          this,  &SeerStaticBrowserWidget::handleSearchLineEdit);
}

SeerStaticBrowserWidget::~SeerStaticBrowserWidget () {
}

void SeerStaticBrowserWidget::handleText (const QString& text) {

    QApplication::setOverrideCursor(Qt::BusyCursor);

    if (text.startsWith(QString::number(_id) + "^done,symbols={") && text.endsWith("}")) {

        staticTreeWidget->clear();
        staticTreeWidget->setSortingEnabled(false);
        staticTreeWidget->sortByColumn(-1, Qt::AscendingOrder);

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

                staticTreeWidget->addTopLevelItem(item);
            }
        }

    }else{
        // Ignore others.
    }

  //staticTreeWidget->resizeColumnToContents(0);  // Name
  //staticTreeWidget->resizeColumnToContents(1);  // Type
    staticTreeWidget->resizeColumnToContents(2);  // Filename
    staticTreeWidget->resizeColumnToContents(3);  // Line
    staticTreeWidget->resizeColumnToContents(4);  // Fullname
    staticTreeWidget->resizeColumnToContents(5);  // Description
    staticTreeWidget->sortByColumn(0, Qt::AscendingOrder);
    staticTreeWidget->setSortingEnabled(true);

    QApplication::restoreOverrideCursor();
}

void SeerStaticBrowserWidget::handleItemDoubleClicked (QTreeWidgetItem* item, int column) {

    Q_UNUSED(column);

    emit selectedFile(item->text(2), item->text(4), item->text(3).toInt());
}

void SeerStaticBrowserWidget::handleItemEntered (QTreeWidgetItem* item, int column) {

    Q_UNUSED(column);

    QString tip = QString("Variable: %1\nType: %2\nFile: %3\nLine: %4\nFullname: %5\nDescription: %6").arg(item->text(0)).arg(item->text(1)).arg(item->text(2)).arg(item->text(3)).arg(item->text(4)).arg(item->text(5));

    item->setToolTip(0, tip);

    for (int i=1; i<staticTreeWidget->columnCount(); i++) { // Copy tooltip to the other columns.
        item->setToolTip(i, item->toolTip(0));
    }
}

void SeerStaticBrowserWidget::handleSearchLineEdit () {

    staticTreeWidget->clear();
    staticTreeWidget->setSortingEnabled(false);
    staticTreeWidget->sortByColumn(-1, Qt::AscendingOrder);

  //staticTreeWidget->resizeColumnToContents(0);
  //staticTreeWidget->resizeColumnToContents(1);
    staticTreeWidget->resizeColumnToContents(2);
    staticTreeWidget->resizeColumnToContents(3);
    staticTreeWidget->resizeColumnToContents(4);
    staticTreeWidget->resizeColumnToContents(5);

    if (staticNameSearchLineEdit->text() != "" || staticTypeSearchLineEdit->text() != "") {
        emit refreshVariableList(_id, staticNameSearchLineEdit->text(), staticTypeSearchLineEdit->text());
    }
}

void SeerStaticBrowserWidget::refresh () {
    handleSearchLineEdit();
}

