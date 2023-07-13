#include "SeerBreakpointsBrowserWidget.h"
#include "SeerBreakpointCreateDialog.h"
#include "SeerUtl.h"
#include <QtWidgets/QTreeWidget>
#include <QtWidgets/QTreeWidgetItemIterator>
#include <QtWidgets/QApplication>
#include <QtWidgets/QMessageBox>
#include <QtWidgets/QInputDialog>
#include <QtCore/QFileInfo>
#include <QtCore/QDebug>

SeerBreakpointsBrowserWidget::SeerBreakpointsBrowserWidget (QWidget* parent) : QWidget(parent) {

    // Construct the UI.
    setupUi(this);

    // Setup the widgets
    breakpointsTreeWidget->clear();

    breakpointsTreeWidget->setSortingEnabled(false);
    breakpointsTreeWidget->setSelectionMode(QAbstractItemView::ExtendedSelection);
    breakpointsTreeWidget->resizeColumnToContents(0); // number
    breakpointsTreeWidget->resizeColumnToContents(1); // type
    breakpointsTreeWidget->resizeColumnToContents(2); // disp
    breakpointsTreeWidget->resizeColumnToContents(3); // enabled
    breakpointsTreeWidget->resizeColumnToContents(4); // addr
  //breakpointsTreeWidget->resizeColumnToContents(5); // func      Too long to show
    breakpointsTreeWidget->resizeColumnToContents(6); // file
  //breakpointsTreeWidget->resizeColumnToContents(7); // fullname  Too long to show
    breakpointsTreeWidget->resizeColumnToContents(8); // line
    breakpointsTreeWidget->resizeColumnToContents(9); // thread-groups
    breakpointsTreeWidget->resizeColumnToContents(10); // cond
    breakpointsTreeWidget->resizeColumnToContents(11); // times
    breakpointsTreeWidget->resizeColumnToContents(12); // ignore
  //breakpointsTreeWidget->resizeColumnToContents(13); // script   Too long to show
    breakpointsTreeWidget->resizeColumnToContents(14); // original-location

    /*
    breakpointsTreeWidget->setColumnHidden(1, true); // ??? Hide or have a config to hide/show columns.
    breakpointsTreeWidget->setColumnHidden(6, true);
    */

    // Connect things.
    QObject::connect(breakpointsTreeWidget,         &QTreeWidget::itemDoubleClicked,    this,  &SeerBreakpointsBrowserWidget::handleItemDoubleClicked);
    QObject::connect(refreshBreakpointsToolButton,  &QToolButton::clicked,              this,  &SeerBreakpointsBrowserWidget::handleRefreshToolButton);
    QObject::connect(addBreakpointToolButton,       &QToolButton::clicked,              this,  &SeerBreakpointsBrowserWidget::handleAddToolButton);
    QObject::connect(deleteBreakpointsToolButton,   &QToolButton::clicked,              this,  &SeerBreakpointsBrowserWidget::handleDeleteToolButton);
    QObject::connect(enableBreakpointsToolButton,   &QToolButton::clicked,              this,  &SeerBreakpointsBrowserWidget::handleEnableToolButton);
    QObject::connect(disableBreakpointsToolButton,  &QToolButton::clicked,              this,  &SeerBreakpointsBrowserWidget::handleDisableToolButton);
    QObject::connect(conditionBreakpointToolButton, &QToolButton::clicked,              this,  &SeerBreakpointsBrowserWidget::handleConditionToolButton);
    QObject::connect(ignoreBreakpointToolButton,    &QToolButton::clicked,              this,  &SeerBreakpointsBrowserWidget::handleIgnoreToolButton);
    QObject::connect(commandsBreakpointToolButton,  &QToolButton::clicked,              this,  &SeerBreakpointsBrowserWidget::handleCommandsToolButton);
}

SeerBreakpointsBrowserWidget::~SeerBreakpointsBrowserWidget () {
}

bool SeerBreakpointsBrowserWidget::isEmpty() const {

    return (breakpointsTreeWidget->topLevelItemCount() == 0);
}

void SeerBreakpointsBrowserWidget::handleText (const QString& text) {

    // Don't do any work if the widget is hidden.
    if (isHidden()) {
        return;
    }

    QApplication::setOverrideCursor(Qt::BusyCursor);

    if (text.startsWith("^done,BreakpointTable={") && text.endsWith("}")) {

        breakpointsTreeWidget->clear();

        //
        // ^done,BreakpointTable={
        //     nr_rows="2",nr_cols="6",
        //
        //     hdr=[
        //             {width="7",alignment="-1",col_name="number",colhdr="Num"},
        //             {width="14",alignment="-1",col_name="type",colhdr="Type"},
        //             {width="4",alignment="-1",col_name="disp",colhdr="Disp"},
        //             {width="3",alignment="-1",col_name="enabled",colhdr="Enb"},
        //             {width="18",alignment="-1",col_name="addr",colhdr="Address"},
        //             {width="40",alignment="2",col_name="what",colhdr="What"}
        //         ],
        //
        //     body=[
        //             bkpt={number="2",type="breakpoint",disp="keep",enabled="y",addr="0x0000000000400c17",func="main(int, char**)",file="helloworld.cpp",fullname="/home/erniep/Development/Peak/src/Seer/helloworld/helloworld.cpp",line="8",thread-groups=["i1"],times="0",original-location="main"},
        //             bkpt={number="3",
        //                   type="breakpoint",
        //                   disp="keep",
        //                   enabled="y",
        //                   addr="0x0000000000400d72",
        //                   func="function1(std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char> > const&)",
        //                   file="function1.cpp",
        //                   fullname="/home/erniep/Development/Peak/src/Seer/helloworld/function1.cpp",
        //                   line="7",
        //                   thread-groups=["i1"],
        //                   cond="1 == 1",
        //                   times="0",
        //                   script={"print i argc"},
        //                   original-location="function1"}
        //          ]
        // }
        //

        QString newtext = Seer::filterEscapes(text); // Filter escaped characters.

        QString body_text = Seer::parseFirst(newtext, "body=", '[', ']', false);

        //qDebug() << body_text;

        if (body_text != "") {

            QStringList bkpt_list = Seer::parse(newtext, "bkpt=", '{', '}', false);

            for ( const auto& bkpt_text : bkpt_list  ) {
                QString number_text            = Seer::parseFirst(bkpt_text, "number=",            '"', '"', false);
                QString type_text              = Seer::parseFirst(bkpt_text, "type=",              '"', '"', false);
                QString disp_text              = Seer::parseFirst(bkpt_text, "disp=",              '"', '"', false);
                QString enabled_text           = Seer::parseFirst(bkpt_text, "enabled=",           '"', '"', false);
                QString addr_text              = Seer::parseFirst(bkpt_text, "addr=",              '"', '"', false);
                QString func_text              = Seer::parseFirst(bkpt_text, "func=",              '"', '"', false);
                QString file_text              = Seer::parseFirst(bkpt_text, "file=",              '"', '"', false);
                QString fullname_text          = Seer::parseFirst(bkpt_text, "fullname=",          '"', '"', false);
                QString line_text              = Seer::parseFirst(bkpt_text, "line=",              '"', '"', false);
                QString thread_groups_text     = Seer::parseFirst(bkpt_text, "thread-groups=",     '[', ']', false);
                QString cond_text              = Seer::parseFirst(bkpt_text, "cond=",              '"', '"', false);
                QString times_text             = Seer::parseFirst(bkpt_text, "times=",             '"', '"', false);
                QString ignore_text            = Seer::parseFirst(bkpt_text, "ignore=",            '"', '"', false);
                QString script_text            = Seer::parseFirst(bkpt_text, "script=",            '{', '}', false);
                QString original_location_text = Seer::parseFirst(bkpt_text, "original-location=", '"', '"', false);

                // Only look for 'breakpoint' type break points.
                if (type_text != "breakpoint") {
                    continue;
                }

                script_text = Seer::filterBookends(Seer::parseCommaList(script_text, '{', '}'), '"', '"').join('\n');

                // Add the level to the tree.
                QTreeWidgetItem* topItem = new QTreeWidgetItem;
                topItem->setText(0, number_text);
                topItem->setText(1, type_text);
                topItem->setText(2, disp_text);
                topItem->setText(3, enabled_text);
                topItem->setText(4, addr_text);
                topItem->setText(5, func_text);
                topItem->setText(6, QFileInfo(file_text).fileName());
                topItem->setText(7, fullname_text);
                topItem->setText(8, line_text);
                topItem->setText(9, thread_groups_text);
                topItem->setText(10, cond_text);
                topItem->setText(11, times_text);
                topItem->setText(12, ignore_text);
                topItem->setText(13, script_text);
                topItem->setText(14, original_location_text);

                for (int i=0; i<topItem->columnCount(); i++) {
                    topItem->setTextAlignment(i, Qt::AlignLeft|Qt::AlignTop);
                }

                breakpointsTreeWidget->addTopLevelItem(topItem);
            }
        }

    }else if (text.startsWith("^error,msg=\"No registers.\"")) {
        // Ignore.

    }else{
        // Ignore others.
    }

    breakpointsTreeWidget->resizeColumnToContents(0);
    breakpointsTreeWidget->resizeColumnToContents(1);
    breakpointsTreeWidget->resizeColumnToContents(2);
    breakpointsTreeWidget->resizeColumnToContents(3);
    breakpointsTreeWidget->resizeColumnToContents(4);
  //breakpointsTreeWidget->resizeColumnToContents(5);
    breakpointsTreeWidget->resizeColumnToContents(6);
  //breakpointsTreeWidget->resizeColumnToContents(7);
    breakpointsTreeWidget->resizeColumnToContents(8);
    breakpointsTreeWidget->resizeColumnToContents(9);
    breakpointsTreeWidget->resizeColumnToContents(10);
    breakpointsTreeWidget->resizeColumnToContents(11);
    breakpointsTreeWidget->resizeColumnToContents(12);
  //breakpointsTreeWidget->resizeColumnToContents(13);
    breakpointsTreeWidget->resizeColumnToContents(14);

    QApplication::restoreOverrideCursor();
}

void SeerBreakpointsBrowserWidget::handleStoppingPointReached () {

    // Don't do any work if the widget is hidden.
    if (isHidden()) {
        return;
    }

    emit refreshBreakpointsList();
}

void SeerBreakpointsBrowserWidget::handleItemDoubleClicked (QTreeWidgetItem* item, int column) {

    Q_UNUSED(column);

    int lineno = item->text(8).toInt();

    emit selectedFile(item->text(6), item->text(7), lineno);
    emit selectedAddress(item->text(4));
}

void SeerBreakpointsBrowserWidget::handleRefreshToolButton () {

    emit refreshBreakpointsList();
}

void SeerBreakpointsBrowserWidget::handleAddToolButton () {

    SeerBreakpointCreateDialog dlg(this);

    int ret = dlg.exec();

    if (ret == 0) {
        return;
    }

    // Build a breakpoint specification.
    QString breakpointParameters = dlg.breakpointText();

    // If nothing, just return.
    if (breakpointParameters == "") {
        return;
    }

    // Otherwise send the command to create the breakpoint.
    emit insertBreakpoint(breakpointParameters);
}

void SeerBreakpointsBrowserWidget::handleDeleteToolButton () {

    // Get selected tree items.
    QList<QTreeWidgetItem*> items =  breakpointsTreeWidget->selectedItems();

    // Build a string that is a list of breakpoints.
    QString breakpoints;

    QList<QTreeWidgetItem*>::iterator i;
    for (i = items.begin(); i != items.end(); ++i) {
        if (i != items.begin()) {
            breakpoints += " ";
        }
        breakpoints += (*i)->text(0);
    }

    // Don't do anything if the list of breakpoints is empty.
    if (breakpoints == "") {
        return;
    }

    // Send the signal.
    emit deleteBreakpoints(breakpoints);
}

void SeerBreakpointsBrowserWidget::handleEnableToolButton () {

    // Get selected tree items.
    QList<QTreeWidgetItem*> items = breakpointsTreeWidget->selectedItems();

    // Build a string that is a list of breakpoints.
    QString breakpoints;

    QList<QTreeWidgetItem*>::iterator i;
    for (i = items.begin(); i != items.end(); ++i) {

        if (i != items.begin()) {
            breakpoints += " ";
        }

        breakpoints += (*i)->text(0);
    }

    // Don't do anything if the list of breakpoints is empty.
    if (breakpoints == "") {
        return;
    }

    // Send the signal.
    emit enableBreakpoints(breakpoints);
}

void SeerBreakpointsBrowserWidget::handleDisableToolButton () {

    // Get selected tree items.
    QList<QTreeWidgetItem*> items = breakpointsTreeWidget->selectedItems();

    // Build a string that is a list of breakpoints.
    QString breakpoints;

    QList<QTreeWidgetItem*>::iterator i;
    for (i = items.begin(); i != items.end(); ++i) {

        if (i != items.begin()) {
            breakpoints += " ";
        }

        breakpoints += (*i)->text(0);
    }

    // Don't do anything if the list of breakpoints is empty.
    if (breakpoints == "") {
        return;
    }

    // Send the signal.
    emit disableBreakpoints(breakpoints);
}

void SeerBreakpointsBrowserWidget::handleConditionToolButton () {

    // Get selected tree items. Only allow one.
    QList<QTreeWidgetItem*> items = breakpointsTreeWidget->selectedItems();

    if (items.count() == 0) {
        return;
    }

    if (items.count() > 1) {
        QMessageBox::warning(this, "Seer", "Select only one breakpoint when adding a condition.", QMessageBox::Ok);
        return;
    }

    // Get the condition text.
    bool ok;
    QString condition = QInputDialog::getText(this, "Seer", "Enter the condition for this breakpoint.\nA blank condition will remove an existing one.", QLineEdit::Normal, items.front()->text(10), &ok);

    if (ok == false) {
        return;
    }

    // Get the selected breakpoint number.
    QString breakpoint = items.front()->text(0);

    // Send the signal.
    emit addBreakpointCondition(breakpoint, condition);
}

void SeerBreakpointsBrowserWidget::handleIgnoreToolButton () {

    // Get selected tree items. Only allow one.
    QList<QTreeWidgetItem*> items = breakpointsTreeWidget->selectedItems();

    if (items.count() == 0) {
        return;
    }

    if (items.count() > 1) {
        QMessageBox::warning(this, "Seer", "Select only one breakpoint when adding an ignore count.", QMessageBox::Ok);
        return;
    }

    // Get the ignore text.
    bool ok;
    int count = QInputDialog::getInt(this, "Seer", "Enter the ignore count for this breakpoint.\nA count of 0 will remove an existing one.", items.front()->text(12).toInt(), 0, 2147483647, 1, &ok);

    if (ok == false) {
        return;
    }

    // Get the selected breakpoint number.
    QString breakpoint = items.front()->text(0);

    // Send the signal.
    emit addBreakpointIgnore(breakpoint, QString::number(count));
}

void SeerBreakpointsBrowserWidget::handleCommandsToolButton () {

    // Get selected tree items. Only allow one.
    QList<QTreeWidgetItem*> items = breakpointsTreeWidget->selectedItems();

    if (items.count() == 0) {
        return;
    }

    if (items.count() > 1) {
        QMessageBox::warning(this, "Seer", "Select only one breakpoint when adding commands.", QMessageBox::Ok);
        return;
    }

    // Get the ignore text.
    bool ok;
    QString commandstr = QInputDialog::getMultiLineText(this, "Seer", "Enter the commands to execute for this breakpoint.\nA blank list will remove existing ones.", items.front()->text(13), &ok);

    if (ok == false) {
        return;
    }

    // Get the selected breakpoint number.
    QString breakpoint = items.front()->text(0);

    QStringList commands = Seer::quoteChars(commandstr.split('\n', Qt::SkipEmptyParts), "\"");

    // Send the signal.
    emit addBreakpointCommands(breakpoint, commands);
}

void SeerBreakpointsBrowserWidget::showEvent (QShowEvent* event) {

    QWidget::showEvent(event);

    emit refreshBreakpointsList();
}

