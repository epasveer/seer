// SPDX-FileCopyrightText: 2026 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "SeerTracepointOutputBrowserWidget.h"
#include "SeerTracepointsBrowserWidget.h"
#include "SeerHelpPageDialog.h"
#include "SeerUtl.h"
#include <QtWidgets/QTreeWidgetItem>
#include <QtCore/QFileInfo>
#include <QtCore/QStringList>
#include <QtCore/QSettings>

// Up to this many snapshots are looked up without being asked.
static const int AutoScanLimit = 100;

static const char* NoTracepoint = "Select a tracepoint.";

SeerTracepointOutputBrowserWidget::SeerTracepointOutputBrowserWidget (QWidget* parent) : QWidget(parent) {

    _traceRunning  = false;
    _traceFrames   = 0;
    _traceFrame    = -1;
    _scanning      = false;
    _scannedFrames = -1;

    // Construct the UI.
    setupUi(this);

    // Setup the widgets
    tracepointLabel->setTextFormat(Qt::PlainText);
    tracepointLabel->setText(NoTracepoint);

    // Single column lists. They don't need to scroll sideways.
    tracepointsTreeWidget->setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
    snapshotsTreeWidget->setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);

    splitter->setStretchFactor(0, 1);
    splitter->setStretchFactor(1, 1);
    splitter->setStretchFactor(2, 6);

    // Restore the splitter positions.
    readSettings();

    updateViewing();
    updateButtons();

    // Connect things.
    QObject::connect(tracepointsTreeWidget, &QTreeWidget::itemSelectionChanged, this,  &SeerTracepointOutputBrowserWidget::handleTracepointClicked);
    QObject::connect(snapshotsTreeWidget,   &QTreeWidget::itemClicked,      this,  &SeerTracepointOutputBrowserWidget::handleSnapshotClicked);
    QObject::connect(snapshotsTreeWidget,   &QTreeWidget::itemActivated,    this,  &SeerTracepointOutputBrowserWidget::handleSnapshotClicked);
    QObject::connect(splitter,              &QSplitter::splitterMoved,      this,  &SeerTracepointOutputBrowserWidget::handleSplitterMoved);
    QObject::connect(helpToolButton,        &QToolButton::clicked,          this,  &SeerTracepointOutputBrowserWidget::handleHelpToolButton);
    QObject::connect(loadAllToolButton,     &QToolButton::clicked,          this,  [this]() { scanSnapshots(true); });
    QObject::connect(goToolButton,          &QToolButton::clicked,          this,  [this]() { emit findTraceFrame(frameSpinBox->value()); });
    QObject::connect(liveToolButton,        &QToolButton::clicked,          this,  [this]() { emit findTraceFrame(-1); });

    QObject::connect(firstToolButton,       &QToolButton::clicked,          this,  [this]() {
        const QList<int> frames = snapshots();
        if (frames.isEmpty() == false) {
            emit findTraceFrame(frames.first());
        }
    });

    QObject::connect(lastToolButton,        &QToolButton::clicked,          this,  [this]() {
        const QList<int> frames = snapshots();
        if (frames.isEmpty() == false) {
            emit findTraceFrame(frames.last());
        }
    });

    QObject::connect(previousToolButton,    &QToolButton::clicked,          this,  [this]() {
        int previous = -1;
        const QList<int> frames = snapshots();
        for (int frame : frames) {
            if (frame < _traceFrame) {
                previous = frame;
            }
        }
        if (previous >= 0) {
            emit findTraceFrame(previous);
        }
    });

    QObject::connect(nextToolButton,        &QToolButton::clicked,          this,  [this]() {
        const QList<int> frames = snapshots();
        for (int frame : frames) {
            if (frame > _traceFrame) {
                emit findTraceFrame(frame);
                break;
            }
        }
    });
}

SeerTracepointOutputBrowserWidget::~SeerTracepointOutputBrowserWidget () {
}

void SeerTracepointOutputBrowserWidget::handleText (const QString& text) {

    // Track the selected snapshot and the trace status, even when hidden.
    if (text.startsWith("=traceframe-changed,")) {

        // From a 'tfind' entered manually.
        //
        // =traceframe-changed,num="0",tracepoint="1"
        // =traceframe-changed,end

        QMap<QString,QString> map = Seer::mapMI(text.section(',', 1));

        if (map.contains("num")) {
            setTraceFrame(map["num"].toInt(), map["tracepoint"]);
        }else{
            setTraceFrame(-1, "");
        }

    }else if (text.startsWith("^done,found=")) {

        // From -trace-find.
        //
        // ^done,found="1",tracepoint="1",traceframe="0",frame={...}
        // ^done,found="0"

        QMap<QString,QString> map = Seer::mapMI(text.section(',', 1));

        if (map["found"] == "1") {
            setTraceFrame(map["traceframe"].toInt(), map["tracepoint"]);
        }else{
            setTraceFrame(-1, "");
        }

    }else if (text.startsWith("^done,supported=")) {

        handleTraceStatus(text);

    }else if (text.startsWith("^done,explicit-variables=")) {

        handleTraceCollected(text);

    }else if (text.startsWith("^done,BreakpointTable={") && text.endsWith("}")) {

        // Don't do any work if the widget is hidden.
        if (isHidden()) {
            return;
        }

        handleTracepointTable(text);
    }
}

void SeerTracepointOutputBrowserWidget::handleTracepointTable (const QString& text) {

    // See SeerTracepointsBrowserWidget.cpp for the table.
    QMap<QString,QString> table = Seer::mapMI(Seer::mapMI(text.section(',', 1))["BreakpointTable"]);

    const QList<QStringPair> bkpts = Seer::splitMI(table["body"]);

    bool found = false;

    tracepointsTreeWidget->blockSignals(true);
    tracepointsTreeWidget->clear();

    for (const auto& bkpt : bkpts) {

        QMap<QString,QString> map = Seer::mapMI(bkpt.second);

        if (map["type"].endsWith("tracepoint") == false) {
            continue;
        }

        QTreeWidgetItem* item = new QTreeWidgetItem;
        item->setText(0, map["number"]);
        item->setToolTip(0, map["func"] != "" ? map["func"] + " at " + QFileInfo(map["file"]).fileName() + ":" + map["line"] : map["original-location"]);

        tracepointsTreeWidget->addTopLevelItem(item);

        if (map["number"] != _tracepoint) {
            continue;
        }

        found = true;

        tracepointsTreeWidget->setCurrentItem(item);

        QString label = "Tracepoint " + _tracepoint + ": ";

        if (map["func"] != "") {
            label += map["func"] + " at ";
        }

        if (map["file"] != "") {
            label += QFileInfo(map["file"]).fileName() + ":" + map["line"];
        }else{
            label += map["original-location"];
        }

        if (map["cond"] != "") {
            label += "   if " + map["cond"];
        }

        if (map["pass"] != "") {
            label += "   pass count " + map["pass"];
        }

        if (map["enabled"] == "n") {
            label += "   (disabled)";
        }

        QStringList actions;

        const QList<QStringPair> scripts = Seer::splitMI(map["script"]);

        for (const auto& script : scripts) {
            actions.append(script.second.trimmed());
        }

        if (actions.isEmpty() == false) {
            label += "\n" + actions.join("; ");
        }

        tracepointLabel->setText(label);
    }

    tracepointsTreeWidget->blockSignals(false);

    // It's been deleted.
    if (found == false && _tracepoint != "") {
        handleTracepointSelected("");
    }
}

void SeerTracepointOutputBrowserWidget::handleTracepointClicked () {

    const QList<QTreeWidgetItem*> items = tracepointsTreeWidget->selectedItems();

    QString tracepoint = (items.isEmpty() ? QString() : items.front()->text(0));

    if (tracepoint == _tracepoint) {
        return;
    }

    handleTracepointSelected(tracepoint);

    emit tracepointSelected(tracepoint);    // Tell the Tracepoints tab.
}

void SeerTracepointOutputBrowserWidget::handleTraceStatus (const QString& text) {

    // See SeerTracepointsBrowserWidget.cpp for the status.
    QMap<QString,QString> map = Seer::mapMI(text.section(',', 1));

    // A new trace run discards the old snapshots. A circular buffer discards them as it goes.
    if (map["start-time"] != _traceStartTime || map["frames"].toInt() < _traceFrames || (map["circular"] == "1" && map["running"] == "1")) {
        _owners.clear();
        _scannedFrames = -1;
    }

    _traceStartTime = map["start-time"];
    _traceRunning   = (map["running"] == "1");
    _traceFrames    = map["frames"].toInt();

    statusLabel->setText(SeerTracepointsBrowserWidget::traceStatusText(map));

    refreshSnapshots();
    updateButtons();

    scanSnapshots(false);
}

void SeerTracepointOutputBrowserWidget::handleTraceCollected (const QString& text) {

    // ^done,explicit-variables=[{name="v",value="0x7ffffffbdad0"},...],
    //       computed-expressions=[{name="v[0]@8",value="{...}"}],
    //       registers=[{number="0",value="0x7ffffffbdad0"},...],
    //       tvars=[{name="$trace_timestamp",current="1"}],memory=[...]
    //
    // Registers only have numbers. They're shown by Seer's register browser instead.

    collectedTreeWidget->clear();

    // A late reply, after moving elsewhere.
    if (viewingTracepoint() == false) {
        return;
    }

    QMap<QString,QString> map = Seer::mapMI(text.section(',', 1));

    const QStringList sections = {"explicit-variables", "computed-expressions", "tvars"};

    for (const auto& section : sections) {

        const QList<QStringPair> entries = Seer::splitMI(map[section]);

        for (const auto& entry : entries) {

            QMap<QString,QString> variable = Seer::mapMI(entry.second);

            QTreeWidgetItem* item = new QTreeWidgetItem;
            item->setText(0, variable["name"]);
            item->setText(1, variable.contains("value") ? variable["value"] : variable["current"]);

            collectedTreeWidget->addTopLevelItem(item);
        }
    }

    collectedTreeWidget->resizeColumnToContents(0);
}

void SeerTracepointOutputBrowserWidget::setTraceFrame (int frameno, const QString& tracepoint) {

    _traceFrame = frameno;

    if (_traceFrame >= 0) {

        _owners[_traceFrame] = tracepoint;

        frameSpinBox->setMaximum(qMax(frameSpinBox->maximum(), _traceFrame));
        frameSpinBox->setValue(_traceFrame);
    }

    refreshSnapshots();
    updateViewing();
    updateButtons();
}

void SeerTracepointOutputBrowserWidget::refreshSnapshots () {

    const QList<int> frames = snapshots();

    snapshotsTreeWidget->clear();

    for (int frame : frames) {

        QTreeWidgetItem* item = new QTreeWidgetItem;
        item->setText(0, QString::number(frame));
        item->setData(0, Qt::UserRole, frame);

        snapshotsTreeWidget->addTopLevelItem(item);

        if (frame == _traceFrame) {
            snapshotsTreeWidget->setCurrentItem(item);
        }
    }
}

void SeerTracepointOutputBrowserWidget::scanSnapshots (bool all) {

    // Snapshots can't be looked at while tracing.
    if (_scanning || _traceRunning || _traceFrames <= 0) {
        return;
    }

    // Don't do any work unless asked to, or there's only a few.
    if (all == false && (isHidden() || _tracepoint == "" || _traceFrames > AutoScanLimit || _traceFrames == _scannedFrames)) {
        return;
    }

    QList<int> frames;

    for (int i=0; i<_traceFrames; i++) {
        if (_owners.contains(i) == false) {
            frames.append(i);
        }
    }

    if (frames.isEmpty()) {
        return;
    }

    _scanning      = true;
    _scannedFrames = _traceFrames;

    updateButtons();

    emit scanTraceFrames(frames, _traceFrame);
}

void SeerTracepointOutputBrowserWidget::handleTraceFrameScanned (int frameno, QString tracepoint) {

    _owners[frameno] = tracepoint;
}

void SeerTracepointOutputBrowserWidget::handleTraceScanFinished () {

    _scanning = false;

    refreshSnapshots();
    updateButtons();
}

bool SeerTracepointOutputBrowserWidget::viewingTracepoint () const {

    return (_traceFrame >= 0 && _tracepoint != "" && _owners.value(_traceFrame) == _tracepoint);
}

QList<int> SeerTracepointOutputBrowserWidget::snapshots () const {

    if (_tracepoint == "") {
        return QList<int>();
    }

    return _owners.keys(_tracepoint);   // Keys are in order.
}

void SeerTracepointOutputBrowserWidget::updateViewing () {

    collectedTreeWidget->clear();

    if (_traceFrame < 0) {

        viewingLabel->setText("Viewing: live program");

    }else if (viewingTracepoint()) {

        viewingLabel->setText(QString("<b>Viewing: snapshot %1</b>").arg(_traceFrame));

        if (isHidden() == false) {
            emit refreshTraceCollected();
        }

    }else{

        viewingLabel->setText(QString("<b>Viewing: snapshot %1, from tracepoint %2</b>").arg(_traceFrame).arg(_owners.value(_traceFrame)));
    }
}

void SeerTracepointOutputBrowserWidget::updateButtons () {

    const QList<int> frames = snapshots();

    bool browse = (_traceFrames > 0 && _traceRunning == false && _scanning == false);
    bool own    = (browse && frames.isEmpty() == false);

    frameSpinBox->setMaximum(qMax(0, qMax(_traceFrames-1, _traceFrame)));

    firstToolButton->setEnabled(own);
    previousToolButton->setEnabled(own && frames.first() < _traceFrame);
    nextToolButton->setEnabled(own && frames.last() > _traceFrame);
    lastToolButton->setEnabled(own);
    frameSpinBox->setEnabled(browse);
    goToolButton->setEnabled(browse);
    liveToolButton->setEnabled(_traceFrame >= 0);
    loadAllToolButton->setEnabled(browse && _tracepoint != "" && _owners.count() < _traceFrames);
}

void SeerTracepointOutputBrowserWidget::handleTracepointSelected (QString tracepoint) {

    _tracepoint = tracepoint;

    // Match the selection in the list.
    tracepointsTreeWidget->blockSignals(true);
    tracepointsTreeWidget->clearSelection();

    for (int i=0; i<tracepointsTreeWidget->topLevelItemCount(); i++) {
        if (tracepointsTreeWidget->topLevelItem(i)->text(0) == _tracepoint) {
            tracepointsTreeWidget->setCurrentItem(tracepointsTreeWidget->topLevelItem(i));
        }
    }

    tracepointsTreeWidget->blockSignals(false);

    tracepointLabel->setText(_tracepoint == "" ? QString(NoTracepoint) : "Tracepoint " + _tracepoint);

    refreshSnapshots();
    updateViewing();
    updateButtons();

    // Get the details.
    if (isHidden() == false) {
        emit refreshTracepointsList();
        emit refreshTraceStatus();
    }
}

void SeerTracepointOutputBrowserWidget::handleStoppingPointReached () {

    // Don't do any work if the widget is hidden.
    if (isHidden()) {
        return;
    }

    emit refreshTracepointsList();
    emit refreshTraceStatus();
}

void SeerTracepointOutputBrowserWidget::handleSessionTerminated () {

    _traceStartTime = "";
    _traceRunning   = false;
    _traceFrames    = 0;
    _scanning       = false;
    _scannedFrames  = -1;
    _owners.clear();

    statusLabel->setText("");
    tracepointsTreeWidget->clear();

    setTraceFrame(-1, "");
    handleTracepointSelected("");
}

void SeerTracepointOutputBrowserWidget::handleSnapshotClicked (QTreeWidgetItem* item, int column) {

    Q_UNUSED(column);

    // Read it now. The list is rebuilt when gdb replies.
    int frame = item->data(0, Qt::UserRole).toInt();

    if (frame != _traceFrame) {
        emit findTraceFrame(frame);
    }
}

void SeerTracepointOutputBrowserWidget::handleSplitterMoved (int pos, int index) {

    Q_UNUSED(pos);
    Q_UNUSED(index);

    writeSettings();
}

void SeerTracepointOutputBrowserWidget::writeSettings () {

    QSettings settings;

    settings.beginGroup("tracepointoutputwindow"); {
        settings.setValue("splitter", splitter->saveState());
    } settings.endGroup();
}

void SeerTracepointOutputBrowserWidget::readSettings () {

    QSettings settings;

    settings.beginGroup("tracepointoutputwindow"); {
        if (settings.contains("splitter")) {
            splitter->restoreState(settings.value("splitter").toByteArray());
        }
    } settings.endGroup();
}

void SeerTracepointOutputBrowserWidget::handleHelpToolButton () {

    SeerHelpPageDialog* help = new SeerHelpPageDialog;
    help->loadFile(":/seer/resources/help/Tracepoints.md");
    help->show();
    help->raise();
}

void SeerTracepointOutputBrowserWidget::showEvent (QShowEvent* event) {

    QWidget::showEvent(event);

    emit refreshTracepointsList();
    emit refreshTraceStatus();

    if (viewingTracepoint()) {
        emit refreshTraceCollected();
    }
}
