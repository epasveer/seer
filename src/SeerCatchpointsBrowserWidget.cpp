#include "SeerCatchpointsBrowserWidget.h"
#include "SeerCatchpointCreateDialog.h"
#include "SeerUtl.h"
#include <QtWidgets/QTreeWidget>
#include <QtWidgets/QTreeWidgetItemIterator>
#include <QtWidgets/QApplication>
#include <QtWidgets/QMessageBox>
#include <QtWidgets/QInputDialog>
#include <QtCore/QDebug>

SeerCatchpointsBrowserWidget::SeerCatchpointsBrowserWidget (QWidget* parent) : QWidget(parent) {

    // Construct the UI.
    setupUi(this);

    // Setup the widgets
    catchpointsTreeWidget->clear();

    catchpointsTreeWidget->setSortingEnabled(false);
    catchpointsTreeWidget->setSelectionMode(QAbstractItemView::ExtendedSelection);
    catchpointsTreeWidget->resizeColumnToContents(0); // number
    catchpointsTreeWidget->resizeColumnToContents(1); // type
    catchpointsTreeWidget->resizeColumnToContents(2); // disp
    catchpointsTreeWidget->resizeColumnToContents(3); // enabled
    catchpointsTreeWidget->resizeColumnToContents(4); // what
    catchpointsTreeWidget->resizeColumnToContents(5); // catch-type
    catchpointsTreeWidget->resizeColumnToContents(6); // name
    catchpointsTreeWidget->resizeColumnToContents(7); // thread-groups
    catchpointsTreeWidget->resizeColumnToContents(8); // cond
    catchpointsTreeWidget->resizeColumnToContents(9); // times
    catchpointsTreeWidget->resizeColumnToContents(10); // ignore
  //catchpointsTreeWidget->resizeColumnToContents(11); // script   Too long to show

    catchpointsTreeWidget->setColumnHidden(12, true); // Hide the 'used' column.
    catchpointsTreeWidget->clear();

    // Connect things.
    QObject::connect(refreshCatchpointsToolButton,  &QToolButton::clicked,              this,  &SeerCatchpointsBrowserWidget::handleRefreshToolButton);
    QObject::connect(addCatchpointToolButton,       &QToolButton::clicked,              this,  &SeerCatchpointsBrowserWidget::handleAddToolButton);
    QObject::connect(deleteCatchpointsToolButton,   &QToolButton::clicked,              this,  &SeerCatchpointsBrowserWidget::handleDeleteToolButton);
    QObject::connect(enableCatchpointsToolButton,   &QToolButton::clicked,              this,  &SeerCatchpointsBrowserWidget::handleEnableToolButton);
    QObject::connect(disableCatchpointsToolButton,  &QToolButton::clicked,              this,  &SeerCatchpointsBrowserWidget::handleDisableToolButton);
    QObject::connect(conditionCatchpointToolButton, &QToolButton::clicked,              this,  &SeerCatchpointsBrowserWidget::handleConditionToolButton);
    QObject::connect(ignoreCatchpointToolButton,    &QToolButton::clicked,              this,  &SeerCatchpointsBrowserWidget::handleIgnoreToolButton);
    QObject::connect(commandsCatchpointToolButton,  &QToolButton::clicked,              this,  &SeerCatchpointsBrowserWidget::handleCommandsToolButton);
}

SeerCatchpointsBrowserWidget::~SeerCatchpointsBrowserWidget () {
}

bool SeerCatchpointsBrowserWidget::isEmpty() const {

    return (catchpointsTreeWidget->topLevelItemCount() == 0);
}

void SeerCatchpointsBrowserWidget::handleText (const QString& text) {

    // Don't do any work if the widget is hidden.
    if (isHidden()) {
        return;
    }

    //qDebug() << text;

    QApplication::setOverrideCursor(Qt::BusyCursor);

    if (text.startsWith("^done,BreakpointTable={") && text.endsWith("}")) {

        // ^done,BreakpointTable={
        //    nr_rows="3", nr_cols="6",
        //
        //    hdr=[
        //            {width="7",alignment="-1",col_name="number",colhdr="Num"},
        //            {width="14",alignment="-1",col_name="type",colhdr="Type"},
        //            {width="4",alignment="-1",col_name="disp",colhdr="Disp"},
        //            {width="3",alignment="-1",col_name="enabled",colhdr="Enb"},
        //            {width="18",alignment="-1",col_name="addr",colhdr="Address"},
        //            {width="40",alignment="2",col_name="what",colhdr="What"}
        //        ],
        //
        //    body=[
        //            bkpt={number="2",type="catchpoint",disp="keep",enabled="y",what="exception catch",catch-type="catch",thread-groups=["i1"],times="0"},
        //            bkpt={number="3",
        //                  type="catchpoint",
        //                  disp="keep",
        //                  enabled="y",
        //                  what="exception throw",
        //                  catch-type="throw",
        //                  thread-groups=["i1"],
        //                  regexp="Exception*",
        //                  times="0"}
        //          ]
        //

        QString newtext = Seer::filterEscapes(text); // Filter escaped characters.

        QString body_text = Seer::parseFirst(newtext, "body=", '[', ']', false);

        //qDebug() << body_text;

        // No rows? Just clear the tree.
        if (body_text == "") {
            catchpointsTreeWidget->clear();

        // Otherwise, populate it.
        }else{

            // Mark each entry initially as "unused".
            // Later, some will be marked as "reused" or "new". Then the "unused" ones will
            // be deleted.
            QTreeWidgetItemIterator it(catchpointsTreeWidget);
            while (*it) {
                (*it)->setText(12, "unused");
                ++it;
            }

            QStringList bkpt_list = Seer::parse(newtext, "bkpt=", '{', '}', false);

            for ( const auto& bkpt_text : bkpt_list  ) {

                QString number_text            = Seer::parseFirst(bkpt_text, "number=",            '"', '"', false);
                QString type_text              = Seer::parseFirst(bkpt_text, "type=",              '"', '"', false);
                QString disp_text              = Seer::parseFirst(bkpt_text, "disp=",              '"', '"', false);
                QString enabled_text           = Seer::parseFirst(bkpt_text, "enabled=",           '"', '"', false);
                QString what_text              = Seer::parseFirst(bkpt_text, "what=",              '"', '"', false);
                QString catch_type_text        = Seer::parseFirst(bkpt_text, "catch-type=",        '"', '"', false);
                QString name_text              = Seer::parseFirst(bkpt_text, "regexp=",            '"', '"', false);
                QString thread_groups_text     = Seer::parseFirst(bkpt_text, "thread-groups=",     '[', ']', false);
                QString cond_text              = Seer::parseFirst(bkpt_text, "cond=",              '"', '"', false);
                QString times_text             = Seer::parseFirst(bkpt_text, "times=",             '"', '"', false);
                QString ignore_text            = Seer::parseFirst(bkpt_text, "ignore=",            '"', '"', false);
                QString script_text            = Seer::parseFirst(bkpt_text, "script=",            '{', '}', false);

                // Only look for 'catchpoint' type break points.
                if (type_text != "catchpoint") {
                    continue;
                }

                script_text = Seer::filterBookends(Seer::parseCommaList(script_text, '{', '}'), '"', '"').join('\n');

                // Hack for library 'load' and 'unload' catchpoints.
                // Unlike 'catch' catchpoints, the "regexp" field is blank. The
                // name is buried in the "what" field. So extract it.
                //
                //  what="load of library matching libSampVar.so"
                //  what="unload of library matching libSampVar.so"
                //
                if (name_text == "") {

                    QString loadsearch("load of library matching ");
                    QString unloadsearch("unload of library matching ");

                    if (what_text.startsWith(loadsearch)) {
                        name_text = what_text.mid(loadsearch.length());
                    }

                    if (what_text.startsWith(unloadsearch)) {
                        name_text = what_text.mid(unloadsearch.length());
                    }
                }

                // Instead of creating a new tree each time, we will reuse existing items, if they are there.
                // This allows the expanded items to remain expanded.
                QList<QTreeWidgetItem*> matches = catchpointsTreeWidget->findItems(number_text, Qt::MatchExactly, 0);

                // No matches. So can't reuse. Add the new entry.
                if (matches.size() == 0) {

                    // Add the level to the tree.
                    QTreeWidgetItem* topItem = new QTreeWidgetItem;
                    topItem->setText(0, number_text);
                    topItem->setText(1, type_text);
                    topItem->setText(2, disp_text);
                    topItem->setText(3, enabled_text);
                    topItem->setText(4, what_text);
                    topItem->setText(5, catch_type_text);
                    topItem->setText(6, name_text);
                    topItem->setText(7, thread_groups_text);
                    topItem->setText(8, cond_text);
                    topItem->setText(9, times_text);
                    topItem->setText(10, ignore_text);
                    topItem->setText(11, script_text);
                    topItem->setText(12, "new");

                    for (int i=0; i<topItem->columnCount(); i++) {
                        topItem->setTextAlignment(i, Qt::AlignLeft|Qt::AlignTop);
                    }

                    catchpointsTreeWidget->addTopLevelItem(topItem);

                // Found a match. Reuse it.
                }else{

                    QTreeWidgetItem* topItem = matches.takeFirst();

                    topItem->setText(0, number_text);
                    topItem->setText(1, type_text);
                    topItem->setText(2, disp_text);
                    topItem->setText(3, enabled_text);
                    topItem->setText(4, what_text);
                    topItem->setText(5, catch_type_text);
                    topItem->setText(6, name_text);
                    topItem->setText(7, thread_groups_text);
                    topItem->setText(8, cond_text);
                    topItem->setText(9, times_text);
                    topItem->setText(10, ignore_text);
                    topItem->setText(11, script_text);
                    topItem->setText(12, "reused");
                }
            }

            // At this point, there are some new entries, some reused entries, and some unused ones.
            // Delete the unused ones. They are obsolete.
            QList<QTreeWidgetItem*> matches = catchpointsTreeWidget->findItems("unused", Qt::MatchExactly, 12);

            qDeleteAll(matches);
        }

    }else if (text.startsWith("^error,msg=\"No registers.\"")) {
        // Ignore.

    }else{
        // Ignore others.
    }

    catchpointsTreeWidget->resizeColumnToContents(0);
    catchpointsTreeWidget->resizeColumnToContents(1);
    catchpointsTreeWidget->resizeColumnToContents(2);
    catchpointsTreeWidget->resizeColumnToContents(3);
    catchpointsTreeWidget->resizeColumnToContents(4);
    catchpointsTreeWidget->resizeColumnToContents(5);
    catchpointsTreeWidget->resizeColumnToContents(6);
    catchpointsTreeWidget->resizeColumnToContents(7);
    catchpointsTreeWidget->resizeColumnToContents(8);
    catchpointsTreeWidget->resizeColumnToContents(9);
    catchpointsTreeWidget->resizeColumnToContents(10);
  //catchpointsTreeWidget->resizeColumnToContents(11);
    catchpointsTreeWidget->resizeColumnToContents(12);

    QApplication::restoreOverrideCursor();
}

void SeerCatchpointsBrowserWidget::handleStoppingPointReached () {

    // Don't do any work if the widget is hidden.
    if (isHidden()) {
        return;
    }

    emit refreshCatchpointsList();
}

void SeerCatchpointsBrowserWidget::handleRefreshToolButton () {

    emit refreshCatchpointsList();
}

void SeerCatchpointsBrowserWidget::handleAddToolButton () {

    SeerCatchpointCreateDialog dlg(this);

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

void SeerCatchpointsBrowserWidget::handleDeleteToolButton () {

    // Get selected tree items.
    QList<QTreeWidgetItem*> items = catchpointsTreeWidget->selectedItems();

    // Build a string that is a list of catchpoints.
    QString catchpoints;

    QList<QTreeWidgetItem*>::iterator i;
    for (i = items.begin(); i != items.end(); ++i) {
        if (i != items.begin()) {
            catchpoints += " ";
        }
        catchpoints += (*i)->text(0);
    }

    // Don't do anything if the list of catchpoints is empty.
    if (catchpoints == "") {
        return;
    }

    // Send the signal.
    emit deleteCatchpoints(catchpoints);
}

void SeerCatchpointsBrowserWidget::handleEnableToolButton () {

    // Get selected tree items.
    QList<QTreeWidgetItem*> items = catchpointsTreeWidget->selectedItems();

    // Build a string that is a list of catchpoints.
    QString catchpoints;

    QList<QTreeWidgetItem*>::iterator i;
    for (i = items.begin(); i != items.end(); ++i) {

        if (i != items.begin()) {
            catchpoints += " ";
        }

        catchpoints += (*i)->text(0);
    }

    // Don't do anything if the list of catchpoints is empty.
    if (catchpoints == "") {
        return;
    }

    // Send the signal.
    emit enableCatchpoints(catchpoints);
}

void SeerCatchpointsBrowserWidget::handleDisableToolButton () {

    // Get selected tree items.
    QList<QTreeWidgetItem*> items = catchpointsTreeWidget->selectedItems();

    // Build a string that is a list of catchpoints.
    QString catchpoints;

    QList<QTreeWidgetItem*>::iterator i;
    for (i = items.begin(); i != items.end(); ++i) {

        if (i != items.begin()) {
            catchpoints += " ";
        }

        catchpoints += (*i)->text(0);
    }

    // Don't do anything if the list of catchpoints is empty.
    if (catchpoints == "") {
        return;
    }

    // Send the signal.
    emit disableCatchpoints(catchpoints);
}

void SeerCatchpointsBrowserWidget::handleConditionToolButton () {

    // Get selected tree items. Only allow one.
    QList<QTreeWidgetItem*> items = catchpointsTreeWidget->selectedItems();

    if (items.count() == 0) {
        return;
    }

    if (items.count() > 1) {
        QMessageBox::warning(this, "Seer", "Select only one catchpoint when adding a condition.", QMessageBox::Ok);
        return;
    }

    // Get the condition text.
    bool ok;
    QString condition = QInputDialog::getText(this, "Seer", "Enter the condition for this catchpoint.\nA blank condition will remove an existing one.", QLineEdit::Normal, items.front()->text(8), &ok);

    if (ok == false) {
        return;
    }

    // Get the selected catchpoint number.
    QString catchpoint = items.front()->text(0);

    // Send the signal.
    emit addBreakpointCondition(catchpoint, condition);
}

void SeerCatchpointsBrowserWidget::handleIgnoreToolButton () {

    // Get selected tree items. Only allow one.
    QList<QTreeWidgetItem*> items = catchpointsTreeWidget->selectedItems();

    if (items.count() == 0) {
        return;
    }

    if (items.count() > 1) {
        QMessageBox::warning(this, "Seer", "Select only one catchpoint when adding an ignore count.", QMessageBox::Ok);
        return;
    }

    // Get the ignore text.
    bool ok;
    int count = QInputDialog::getInt(this, "Seer", "Enter the ignore count for this catchpoint.\nA count of 0 will remove an existing one.", items.front()->text(10).toInt(), 0, 2147483647, 1, &ok);

    if (ok == false) {
        return;
    }

    // Get the selected catchpoint number.
    QString catchpoint = items.front()->text(0);

    // Send the signal.
    emit addBreakpointIgnore(catchpoint, QString::number(count));
}

void SeerCatchpointsBrowserWidget::handleCommandsToolButton () {

    // Get selected tree items. Only allow one.
    QList<QTreeWidgetItem*> items = catchpointsTreeWidget->selectedItems();

    if (items.count() == 0) {
        return;
    }

    if (items.count() > 1) {
        QMessageBox::warning(this, "Seer", "Select only one watcpoint when adding commands.", QMessageBox::Ok);
        return;
    }

    // Get the ignore text.
    bool ok;
    QString commandstr = QInputDialog::getMultiLineText(this, "Seer", "Enter the commands to execute for this catchpoint.\nA blank list will remove existing ones.", items.front()->text(11), &ok);

    if (ok == false) {
        return;
    }

    // Get the selected catchpoint number.
    QString catchpoint = items.front()->text(0);

    QStringList commands = Seer::quoteChars(commandstr.split('\n', Qt::SkipEmptyParts), "\"");

    // Send the signal.
    emit addBreakpointCommands(catchpoint, commands);
}

void SeerCatchpointsBrowserWidget::showEvent (QShowEvent* event) {

    QWidget::showEvent(event);

    emit refreshCatchpointsList();
}

