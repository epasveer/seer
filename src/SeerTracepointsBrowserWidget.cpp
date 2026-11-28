// SPDX-FileCopyrightText: 2026 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "SeerTracepointsBrowserWidget.h"
#include "SeerTracepointCreateDialog.h"
#include "SeerHelpPageDialog.h"
#include "SeerUtl.h"
#include <QtWidgets/QTreeWidgetItem>
#include <QtWidgets/QMessageBox>
#include <QtWidgets/QInputDialog>
#include <QtWidgets/QFileDialog>
#include <QtCore/QFileInfo>
#include <QtCore/QDir>
#include <QtCore/QLocale>
#include <QtCore/QSet>
#include <QtCore/QMap>

SeerTracepointsBrowserWidget::SeerTracepointsBrowserWidget (QWidget* parent) : QWidget(parent) {

    _traceSupported = "0";
    _traceRunning   = false;
    _traceFrames    = 0;

    // Construct the UI.
    setupUi(this);

    // Setup the widgets
    tracepointsTreeWidget->clear();
    tracepointsTreeWidget->setSortingEnabled(false);
    tracepointsTreeWidget->setSelectionMode(QAbstractItemView::ExtendedSelection);

    updateButtons();

    // Connect things.
    QObject::connect(tracepointsTreeWidget,         &QTreeWidget::itemDoubleClicked,    this,  &SeerTracepointsBrowserWidget::handleItemDoubleClicked);
    QObject::connect(tracepointsTreeWidget,         &QTreeWidget::itemSelectionChanged, this,  &SeerTracepointsBrowserWidget::handleSelectionChanged);
    QObject::connect(tracepointsTreeWidget,         &QTreeWidget::currentItemChanged,   this,  &SeerTracepointsBrowserWidget::handleSelectionChanged);
    QObject::connect(outputPushButton,              &QPushButton::clicked,              this,  &SeerTracepointsBrowserWidget::showTracepointOutput);
    QObject::connect(refreshTracepointsToolButton,  &QToolButton::clicked,              this,  &SeerTracepointsBrowserWidget::handleRefreshToolButton);
    QObject::connect(addTracepointToolButton,       &QToolButton::clicked,              this,  &SeerTracepointsBrowserWidget::handleAddToolButton);
    QObject::connect(deleteTracepointsToolButton,   &QToolButton::clicked,              this,  &SeerTracepointsBrowserWidget::handleDeleteToolButton);
    QObject::connect(enableTracepointsToolButton,   &QToolButton::clicked,              this,  &SeerTracepointsBrowserWidget::handleEnableToolButton);
    QObject::connect(disableTracepointsToolButton,  &QToolButton::clicked,              this,  &SeerTracepointsBrowserWidget::handleDisableToolButton);
    QObject::connect(conditionTracepointToolButton, &QToolButton::clicked,              this,  &SeerTracepointsBrowserWidget::handleConditionToolButton);
    QObject::connect(passcountTracepointToolButton, &QToolButton::clicked,              this,  &SeerTracepointsBrowserWidget::handlePasscountToolButton);
    QObject::connect(actionsTracepointToolButton,   &QToolButton::clicked,              this,  &SeerTracepointsBrowserWidget::handleActionsToolButton);
    QObject::connect(startPushButton,               &QPushButton::clicked,              this,  &SeerTracepointsBrowserWidget::handleStartButton);
    QObject::connect(stopPushButton,                &QPushButton::clicked,              this,  &SeerTracepointsBrowserWidget::stopTrace);
    QObject::connect(savePushButton,                &QPushButton::clicked,              this,  &SeerTracepointsBrowserWidget::handleSaveButton);
    QObject::connect(helpToolButton,                &QToolButton::clicked,              this,  &SeerTracepointsBrowserWidget::handleHelpToolButton);
}

SeerTracepointsBrowserWidget::~SeerTracepointsBrowserWidget () {
}

bool SeerTracepointsBrowserWidget::isEmpty () const {

    return (tracepointsTreeWidget->topLevelItemCount() == 0);
}

void SeerTracepointsBrowserWidget::handleText (const QString& text) {

    // Track the selected snapshot and the trace status, even when hidden.
    // Track the trace status, even when hidden.
    if (text.startsWith("^done,supported=")) {

        handleTraceStatus(text);

    }else if (text.startsWith("^done,BreakpointTable={") && text.endsWith("}")) {

        // Don't do any work if the widget is hidden.
        if (isHidden()) {
            return;
        }

        handleTracepointTable(text);
    }
}

void SeerTracepointsBrowserWidget::handleTracepointTable (const QString& text) {

    // Remember the selection, it's lost when the list is rebuilt.
    QSet<QString> selected;

    const QList<QTreeWidgetItem*> items = tracepointsTreeWidget->selectedItems();

    for (const auto& item : items) {
        selected.insert(item->text(0));
    }

    // The current one could have been chosen in the TracepointOutput tab.
    if (selected.contains(_tracepoint) == false) {
        selected.clear();
        selected.insert(_tracepoint);
    }

    QString          current     = _tracepoint;
    QTreeWidgetItem* currentItem = nullptr;

    tracepointsTreeWidget->blockSignals(true);
    tracepointsTreeWidget->clear();

    //
    // See SeerBreakpointsBrowserWidget.cpp for the table. A tracepoint entry looks like:
    //
    // bkpt={number="1",type="tracepoint",disp="keep",enabled="y",addr="0x0000555555554948",func="fft",file="hellofft.c",
    //       fullname="/.../hellofft.c",line="50",thread-groups=["i1"],cond="n == 8",times="5",traceframe-usage="12645",
    //       script=["collect $args, v[0]@8"],pass="5",installed="y",original-location="fft"}
    //

    QMap<QString,QString> table = Seer::mapMI(Seer::mapMI(text.section(',', 1))["BreakpointTable"]);

    const QList<QStringPair> bkpts = Seer::splitMI(table["body"]);

    for (const auto& bkpt : bkpts) {

        QMap<QString,QString> map = Seer::mapMI(bkpt.second);

        // Only look for 'tracepoint' types. Could also be 'fast tracepoint' or 'static tracepoint'.
        if (map["type"].endsWith("tracepoint") == false) {
            continue;
        }

        // Older gdb's use {} for scripts and newer ones use []. Both have had their brackets removed.
        QStringList actions;

        const QList<QStringPair> scripts = Seer::splitMI(map["script"]);

        for (const auto& script : scripts) {
            actions.append(script.second);
        }

        QString groups;

        const QList<QStringPair> threadGroups = Seer::splitMI(map["thread-groups"]);

        for (const auto& group : threadGroups) {
            groups += (groups == "" ? "" : ",") + group.second;
        }

        QTreeWidgetItem* item = new QTreeWidgetItem;
        item->setText(0, map["number"]);
        item->setText(1, map["type"]);
        item->setText(2, map["disp"]);
        item->setText(3, map["enabled"]);
        item->setText(4, map["addr"]);
        item->setText(5, map["func"]);
        item->setText(6, QFileInfo(map["file"]).fileName());
        item->setText(7, QDir::toNativeSeparators(map["fullname"]));
        item->setData(7, Qt::UserRole, map["fullname"]);
        item->setText(8, map["line"]);
        item->setText(9, actions.join('\n'));
        item->setText(10, groups);
        item->setText(11, map["cond"]);
        item->setText(12, map["times"]);
        item->setText(13, map["pass"]);
        item->setText(14, map["installed"]);
        item->setText(15, map["original-location"]);

        for (int i=0; i<item->columnCount(); i++) {
            item->setTextAlignment(i, Qt::AlignLeft|Qt::AlignTop);
        }

        tracepointsTreeWidget->addTopLevelItem(item);

        if (current != "" && item->text(0) == current) {
            currentItem = item;
        }
    }

    // Restore the selection.
    if (currentItem) {
        tracepointsTreeWidget->setCurrentItem(currentItem);
    }

    for (int i=0; i<tracepointsTreeWidget->topLevelItemCount(); i++) {

        QTreeWidgetItem* item = tracepointsTreeWidget->topLevelItem(i);

        item->setSelected(selected.contains(item->text(0)));
    }

    tracepointsTreeWidget->blockSignals(false);

    handleSelectionChanged();

    for (int i=0; i<tracepointsTreeWidget->columnCount(); i++) {
        if (i != 5 && i != 7) { // Function and fullname are too long to show.
            tracepointsTreeWidget->resizeColumnToContents(i);
        }
    }
}

void SeerTracepointsBrowserWidget::handleTraceStatus (const QString& text) {

    // ^done,supported="0"
    // ^done,supported="1",running="0",stop-reason="passcount",stopping-tracepoint="1",frames="5",frames-created="5",
    //       buffer-size="5242880",buffer-free="5239310",disconnected="0",circular="0",...

    QMap<QString,QString> map = Seer::mapMI(text.section(',', 1));

    _traceSupported = map["supported"];
    _traceRunning   = (map["running"] == "1");
    _traceFrames    = map["frames"].toInt();

    statusLabel->setText(traceStatusText(map));

    updateButtons();
}

QString SeerTracepointsBrowserWidget::traceStatusText (const QMap<QString,QString>& map) {

    if (map["supported"] == "0") {
        return "This target doesn't support tracing. Use gdbserver and Connect mode.";
    }

    QString status;

    if (map["running"] == "1") {
        status = "Running.";
    }else if (map["stop-reason"] != "") {
        status = "Stopped (" + map["stop-reason"];

        if (map["stopping-tracepoint"] != "") {
            status += ", tracepoint " + map["stopping-tracepoint"];
        }

        status += ").";
    }else{
        status = "Not started.";
    }

    status += QString(" %1 snapshots.").arg(map["frames"].toInt());

    if (map["buffer-size"] != "") {
        status += QString(" Buffer: %1 of %2 bytes free.").arg(QLocale().toString(map["buffer-free"].toLongLong()), QLocale().toString(map["buffer-size"].toLongLong()));
    }

    return status;
}

void SeerTracepointsBrowserWidget::updateButtons () {

    bool live   = (_traceSupported == "1");             // A trace file can be browsed, but not started.

    startPushButton->setEnabled(live && _traceRunning == false);
    stopPushButton->setEnabled(live && _traceRunning);
    savePushButton->setEnabled(_traceFrames > 0);
}

void SeerTracepointsBrowserWidget::handleStoppingPointReached () {

    // Don't do any work if the widget is hidden.
    if (isHidden()) {
        return;
    }

    emit refreshTracepointsList();
    emit refreshTraceStatus();
}

void SeerTracepointsBrowserWidget::handleSessionTerminated () {

    // Delete previous contents.
    tracepointsTreeWidget->clear();
    handleSelectionChanged();

    _traceSupported = "0";
    _traceRunning   = false;
    _traceFrames    = 0;

    statusLabel->setText("");

    updateButtons();
}

void SeerTracepointsBrowserWidget::handleItemDoubleClicked (QTreeWidgetItem* item, int column) {

    Q_UNUSED(column);

    emit selectedFile(item->text(6), item->data(7, Qt::UserRole).toString(), item->text(8).toInt());
}

void SeerTracepointsBrowserWidget::handleRefreshToolButton () {

    emit refreshTracepointsList();
    emit refreshTraceStatus();
}

void SeerTracepointsBrowserWidget::handleAddToolButton () {

    SeerTracepointCreateDialog dlg(this);

    if (dlg.exec() == 0) {
        return;
    }

    // If nothing, just return.
    if (dlg.tracepointParameters() == "") {
        return;
    }

    emit insertTracepoint(dlg.tracepointParameters(), dlg.actions(), dlg.passcount());
}

QString SeerTracepointsBrowserWidget::currentTracepoint () const {

    const QList<QTreeWidgetItem*> items = tracepointsTreeWidget->selectedItems();

    if (items.isEmpty()) {
        return "";
    }

    // With several selected, use the current one.
    if (items.contains(tracepointsTreeWidget->currentItem())) {
        return tracepointsTreeWidget->currentItem()->text(0);
    }

    return items.front()->text(0);
}

void SeerTracepointsBrowserWidget::handleSelectionChanged () {

    QString tracepoint = currentTracepoint();

    if (tracepoint != _tracepoint) {
        _tracepoint = tracepoint;
        emit tracepointSelected(_tracepoint);
    }
}

void SeerTracepointsBrowserWidget::selectTracepoint (QString tracepoint) {

    // Chosen in the TracepointOutput tab. If it isn't in the list yet, it's selected when the list is refreshed.
    _tracepoint = tracepoint;

    tracepointsTreeWidget->blockSignals(true);
    tracepointsTreeWidget->clearSelection();

    for (int i=0; i<tracepointsTreeWidget->topLevelItemCount(); i++) {

        QTreeWidgetItem* item = tracepointsTreeWidget->topLevelItem(i);

        if (item->text(0) == tracepoint) {
            tracepointsTreeWidget->setCurrentItem(item);
            item->setSelected(true);
        }
    }

    tracepointsTreeWidget->blockSignals(false);
}

QString SeerTracepointsBrowserWidget::selectedTracepoints () const {

    QStringList tracepoints;

    const QList<QTreeWidgetItem*> items = tracepointsTreeWidget->selectedItems();

    for (const auto& item : items) {
        tracepoints.append(item->text(0));
    }

    return tracepoints.join(' ');
}

QTreeWidgetItem* SeerTracepointsBrowserWidget::selectedTracepoint (const QString& what) {

    // Get selected tree items. Only allow one.
    QList<QTreeWidgetItem*> items = tracepointsTreeWidget->selectedItems();

    if (items.count() == 0) {
        return nullptr;
    }

    if (items.count() > 1) {
        QMessageBox::warning(this, "Seer", "Select only one tracepoint when " + what + ".", QMessageBox::Ok);
        return nullptr;
    }

    return items.front();
}

void SeerTracepointsBrowserWidget::handleDeleteToolButton () {

    QString tracepoints = selectedTracepoints();

    if (tracepoints != "") {
        emit deleteTracepoints(tracepoints);
    }
}

void SeerTracepointsBrowserWidget::handleEnableToolButton () {

    QString tracepoints = selectedTracepoints();

    if (tracepoints != "") {
        emit enableTracepoints(tracepoints);
    }
}

void SeerTracepointsBrowserWidget::handleDisableToolButton () {

    QString tracepoints = selectedTracepoints();

    if (tracepoints != "") {
        emit disableTracepoints(tracepoints);
    }
}

void SeerTracepointsBrowserWidget::handleConditionToolButton () {

    QTreeWidgetItem* item = selectedTracepoint("adding a condition");

    if (item == nullptr) {
        return;
    }

    // The list can be refreshed while the dialog is up, so don't use 'item' afterwards.
    QString tracepoint = item->text(0);

    bool ok;
    QString condition = QInputDialog::getText(this, "Seer", "Enter the condition for this tracepoint.\nA blank condition will remove an existing one.", QLineEdit::Normal, item->text(11), &ok);

    if (ok) {
        emit addBreakpointCondition(tracepoint, condition);
    }
}

void SeerTracepointsBrowserWidget::handlePasscountToolButton () {

    QTreeWidgetItem* item = selectedTracepoint("setting a pass count");

    if (item == nullptr) {
        return;
    }

    QString tracepoint = item->text(0);

    bool ok;
    int count = QInputDialog::getInt(this, "Seer", "Enter the pass count for this tracepoint. Tracing stops after this many hits.\nA count of 0 will remove an existing one.", item->text(13).toInt(), 0, 2147483647, 1, &ok);

    if (ok) {
        emit addTracepointPasscount(tracepoint, count);
    }
}

void SeerTracepointsBrowserWidget::handleActionsToolButton () {

    QTreeWidgetItem* item = selectedTracepoint("editing actions");

    if (item == nullptr) {
        return;
    }

    QString tracepoint = item->text(0);

    bool ok;
    QString actions = QInputDialog::getMultiLineText(this, "Seer", "Enter the actions for this tracepoint, one per line.\nFor example: collect $args, $locals\nA blank list will remove existing ones.", item->text(9), &ok);

    if (ok) {
        emit addBreakpointCommands(tracepoint, Seer::quoteChars(actions.split('\n', Qt::SkipEmptyParts), "\""));
    }
}

void SeerTracepointsBrowserWidget::handleStartButton () {

    if (_traceFrames > 0) {

        int ret = QMessageBox::question(this, "Seer", QString("Starting a new trace discards the %1 existing snapshots.\n\nContinue?").arg(_traceFrames), QMessageBox::Yes|QMessageBox::No, QMessageBox::No);

        if (ret != QMessageBox::Yes) {
            return;
        }
    }

    emit startTrace();
}

void SeerTracepointsBrowserWidget::handleSaveButton () {

    QString filename = QFileDialog::getSaveFileName(this, "Seer - Save snapshots to a file.", "./tracepoint.seer", "Tracepoint files (*.seer);;All files (*.*)", nullptr, QFileDialog::DontUseNativeDialog);

    if (filename != "") {
        emit saveTrace(filename);
    }
}

void SeerTracepointsBrowserWidget::handleHelpToolButton () {

    SeerHelpPageDialog* help = new SeerHelpPageDialog;
    help->loadFile(":/seer/resources/help/Tracepoints.md");
    help->show();
    help->raise();
}

void SeerTracepointsBrowserWidget::showEvent (QShowEvent* event) {

    QWidget::showEvent(event);

    emit refreshTracepointsList();
    emit refreshTraceStatus();
}
