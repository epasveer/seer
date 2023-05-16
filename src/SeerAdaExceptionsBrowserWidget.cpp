#include "SeerAdaExceptionsBrowserWidget.h"
#include "SeerCatchpointCreateDialog.h"
#include "SeerUtl.h"
#include <QtWidgets/QTreeWidget>
#include <QtWidgets/QTreeWidgetItemIterator>
#include <QtWidgets/QLabel>
#include <QtWidgets/QApplication>
#include <QtCore/QFileInfo>
#include <QtCore/Qt>
#include <QtCore/QDebug>

SeerAdaExceptionsBrowserWidget::SeerAdaExceptionsBrowserWidget (QWidget* parent) : QWidget(parent) {

    // Construct the UI.
    setupUi(this);

    // Setup the widgets
    adaExceptionsSearchLineEdit->setPlaceholderText("Search regex...");
    adaExceptionsSearchLineEdit->setClearButtonEnabled(true);
    adaExceptionsTreeWidget->resizeColumnToContents(0);
    adaExceptionsTreeWidget->resizeColumnToContents(1);
    adaExceptionsTreeWidget->clear();

    // Connect things.
    QObject::connect(adaExceptionsSearchLineEdit, &QLineEdit::textChanged,            this,  &SeerAdaExceptionsBrowserWidget::handleSearchLineEdit);
    QObject::connect(addCatchpointToolButton,     &QToolButton::clicked,              this,  &SeerAdaExceptionsBrowserWidget::handleAddCatchpointToolButtonClicked);
}

SeerAdaExceptionsBrowserWidget::~SeerAdaExceptionsBrowserWidget () {
}

void SeerAdaExceptionsBrowserWidget::handleText (const QString& text) {

    // Don't do any work if the widget is hidden.
    if (isHidden()) {
        return;
    }

    QApplication::setOverrideCursor(Qt::BusyCursor);

    if (text.startsWith("^done,ada-exceptions={") && text.endsWith("}")) {

        adaExceptionsTreeWidget->clear();

        // -info-ada-exceptions aint
        // ^done,ada-exceptions={
        //          nr_rows="2",
        //          nr_cols="2",
        //          hdr=[
        //                  {width="1",alignment="-1",col_name="name",colhdr="Name"},
        //                  {width="1",alignment="-1",col_name="address",colhdr="Address"}
        //              ],
        //          body=[
        //                  {name="constraint_error",address="0x0000000000613da0"},
        //                  {name="const.aint_global_e",address="0x0000000000613b00"}
        //               ]
        // }

        QString body_text = Seer::parseFirst(text, "body=", '[', ']', false);

        //qDebug() << body_text;

        QStringList exceptions_list = Seer::parse(body_text, "", '{', '}', false);

        for ( const auto& entry_text : exceptions_list  ) {

            QString name_text    = Seer::parseFirst(entry_text, "name=",    '"', '"', false);
            QString address_text = Seer::parseFirst(entry_text, "address=", '"', '"', false);

            // Add the file to the tree.
            QTreeWidgetItem* item = new QTreeWidgetItem;
            item->setText(0, name_text);
            item->setText(1, address_text);

            adaExceptionsTreeWidget->addTopLevelItem(item);
        }

    }else{
        // Ignore others.
    }

    adaExceptionsTreeWidget->resizeColumnToContents(0);
    adaExceptionsTreeWidget->resizeColumnToContents(1);

    adaExceptionsSearchLineEdit->clear();

    QApplication::restoreOverrideCursor();
}

void SeerAdaExceptionsBrowserWidget::handleSearchLineEdit (const QString& text) {

    // Set everything to a normal font. If there is no search text, unhide everything.
    // If there is search text, hide everything so the matching ones can be unhidden later on.
    QTreeWidgetItemIterator it(adaExceptionsTreeWidget);

    if (*it) {

        QFont f0 = (*it)->font(0);

        f0.setBold(false);

        if (text == "") {
            while (*it) {
                (*it)->setHidden(false); // No search text, unhide everything.
                (*it)->setFont(0,f0);
                ++it;
            }
        }else{
            while (*it) {
                (*it)->setHidden(true); // Has search text, hide everything. Matching items to be unhidden below.
                (*it)->setFont(0,f0);
                ++it;
            }
        }
    }

    // Set selected items to a bold font and unhidden. Move to the first match.
    if (text != "") {

        QList<QTreeWidgetItem*> matches;

        matches = adaExceptionsTreeWidget->findItems(text, Qt::MatchRegularExpression | Qt::MatchRecursive, 0);

        QList<QTreeWidgetItem*>::const_iterator it = matches.begin();
        QList<QTreeWidgetItem*>::const_iterator e  = matches.end();

        if (it != e) {

            adaExceptionsTreeWidget->setCurrentItem(*it);

            QFont f0 = (*it)->font(0);

            f0.setBold(true);

            while (it != e) {
                (*it)->setHidden(false);
                (*it)->setFont(0,f0);
                it++;
            }
        }

        //qDebug() << text << matches.size();
    }

    adaExceptionsTreeWidget->resizeColumnToContents(0);
    adaExceptionsTreeWidget->resizeColumnToContents(1);
}

void SeerAdaExceptionsBrowserWidget::handleAddCatchpointToolButtonClicked () {

    SeerCatchpointCreateDialog dlg(this);
    dlg.setType("exception");

    QTreeWidgetItem* item = adaExceptionsTreeWidget->currentItem();

    if (item != 0) {
        dlg.setNameText(item->text(0));
    }

    int ret = dlg.exec();

    if (ret == 0) {
        return;
    }

    // Build a catchpoint specification.
    QString catchpointParameters = dlg.catchpointText();

    // If nothing, just return.
    if (catchpointParameters == "") {
        return;
    }

    // Otherwise send the command to create the catchpoint.
    emit insertCatchpoint(catchpointParameters);
}

void SeerAdaExceptionsBrowserWidget::refresh () {

    emit refreshAdaExceptions();
}

void SeerAdaExceptionsBrowserWidget::showEvent (QShowEvent* event) {

    QWidget::showEvent(event);

    refresh();
}

