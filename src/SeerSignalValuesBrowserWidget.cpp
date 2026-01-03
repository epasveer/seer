// SPDX-FileCopyrightText: 2026 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "SeerSignalValuesBrowserWidget.h"
#include "SeerSignalEditValueDialog.h"
#include "SeerSignalProfileDialog.h"
#include "SeerUtl.h"
#include <QtWidgets/QMenu>
#include <QtWidgets/QMessageBox>
#include <QtWidgets/QApplication>
#include <QtGui/QClipboard>
#include <QtCore/QSettings>
#include <QtCore/QDebug>


SeerSignalValuesBrowserWidget::SeerSignalValuesBrowserWidget (QWidget* parent) : QWidget(parent) {

    _newProfileAction    = 0;
    _deleteProfileAction = 0;

    // Construct the UI.
    setupUi(this);

    // Setup the widgets
    signalsTreeWidget->setContextMenuPolicy(Qt::CustomContextMenu);
    signalsTreeWidget->resizeColumnToContents(0); // name
    signalsTreeWidget->resizeColumnToContents(1); // stop
    signalsTreeWidget->resizeColumnToContents(2); // print
    signalsTreeWidget->resizeColumnToContents(3); // pass
    signalsTreeWidget->resizeColumnToContents(4); // description
    signalsTreeWidget->resizeColumnToContents(5); // used
    signalsTreeWidget->setColumnHidden(5, true); // Hide the 'used' column.
    signalsTreeWidget->clear();

    signalProfileComboBox->addItem("allsignals");

    _needsSignalNames = true;

    // Preference menu.
    QMenu* menu = new QMenu();

    _newProfileAction    = menu->addAction("New profile");
    _modifyProfileAction = menu->addAction("Modify current profile");
    _deleteProfileAction = menu->addAction("Delete current profile");

    preferencesToolButton->setMenu(menu);
    preferencesToolButton->setPopupMode(QToolButton::InstantPopup);

    // Connect things.
    QObject::connect(signalsTreeWidget,               &QTreeWidget::itemDoubleClicked,                           this, &SeerSignalValuesBrowserWidget::handleItemDoubleClicked);
    QObject::connect(signalsTreeWidget,               &QTreeWidget::itemEntered,                                 this, &SeerSignalValuesBrowserWidget::handleItemEntered);
    QObject::connect(signalsTreeWidget,               &QTreeWidget::customContextMenuRequested,                  this, &SeerSignalValuesBrowserWidget::handleContextMenu);
    QObject::connect(_newProfileAction,               &QAction::triggered,                                       this, &SeerSignalValuesBrowserWidget::handleNewProfile);
    QObject::connect(_modifyProfileAction,            &QAction::triggered,                                       this, &SeerSignalValuesBrowserWidget::handleModifyProfile);
    QObject::connect(_deleteProfileAction,            &QAction::triggered,                                       this, &SeerSignalValuesBrowserWidget::handleDeleteProfile);
    QObject::connect(signalProfileComboBox,           QOverload<int>::of(&QComboBox::currentIndexChanged),       this, &SeerSignalValuesBrowserWidget::handleProfileChanged);

    // Restore settings.
    readSettings();
}

SeerSignalValuesBrowserWidget::~SeerSignalValuesBrowserWidget () {
}

void SeerSignalValuesBrowserWidget::handleText (const QString& text) {

    // Don't do any work if the widget is hidden.
    if (isHidden()) {
        return;
    }

    QApplication::setOverrideCursor(Qt::BusyCursor);

    if (text.startsWith("^done,signal-names=[") && text.endsWith("]")) {

        // ^done,signal-names=[
        //                  "SIGHUP","SIGINT","SIGQUIT","SIGILL","SIGTRAP","SIGABRT","SIGEMT","SIGFPE","SIGKILL","SIGBUS","SIGSEGV"
        //               ]

        // This recreates the tree.
        signalsTreeWidget->clear();

        QString newtext = Seer::filterEscapes(text); // Filter escaped characters.

        QString frame_text = Seer::parseFirst(newtext, "signal-names=", '[', ']', false);

        QStringList name_list = Seer::parse(frame_text, "", '"', '"', false);

        for ( const auto& name_text : name_list  ) {

            QTreeWidgetItem* topItem = new QTreeWidgetItem;
            topItem->setText(0, name_text);
            topItem->setText(5, "new");

            signalsTreeWidget->addTopLevelItem(topItem);
        }

        handleShowHideSignals();

        _needsSignalNames = false;

    } else if (text.startsWith("^done,signal-values=[") && text.endsWith("]")) {

        // Mark each entry initially as "unused".
        // Later, some will be marked as "reused" or "new". Then the "unused" ones will
        // be deleted.
        QTreeWidgetItemIterator it(signalsTreeWidget);
        while (*it) {
            (*it)->setText(5, "unused");
            ++it;
        }

        // ^done,signal-values=[
        //                  {name="SIGHUP",stop="Yes",print="Yes",pass="Yes",description="Hangup"},
        //                  {name="SIGINT",stop="Yes",print="Yes",pass="No",description="Interrupt"},
        //                  {name="SIGQUIT",stop="Yes",print="Yes",pass="Yes",description="Quit"},
        //                  {name="SIGILL",stop="Yes",print="Yes",pass="Yes",description="Illegal instruction"},
        //                  {name="EXC_EMULATION",stop="Yes",print="Yes",pass="Yes",description="Emulation instruction"},
        //                  {name="EXC_SOFTWARE",stop="Yes",print="Yes",pass="Yes",description="Software generated exception"},
        //                  {name="EXC_BREAKPOINT",stop="Yes",print="Yes",pass="Yes",description="Breakpoint"},
        //                  {name="SIGLIBRT",stop="No",print="No",pass="Yes",description="librt internal signal"}
        //               ]

        QString signals_text = Seer::parseFirst(text, "signal-values=", '[', ']', false);

        QStringList signals_list = Seer::parse(signals_text, "", '{', '}', false);

        for (const auto& signal_entry : signals_list) {

            QString name_text        = Seer::parseFirst(signal_entry, "name=",        '"', '"', false);
            QString stop_text        = Seer::parseFirst(signal_entry, "stop=",        '"', '"', false);
            QString print_text       = Seer::parseFirst(signal_entry, "print=",       '"', '"', false);
            QString pass_text        = Seer::parseFirst(signal_entry, "pass=",        '"', '"', false);
            QString description_text = Seer::parseFirst(signal_entry, "description=", '"', '"', false);

            // Instead of creating a new tree each time, we will reuse existing items, if they are there.
            // This allows the expanded items to remain expanded.
            QList<QTreeWidgetItem*> matches = signalsTreeWidget->findItems(name_text, Qt::MatchExactly, 0);

            // Found a match. Reuse it. Set the value.
            // If no matches, do not add this entry.
            if (matches.size() > 0) {

                QTreeWidgetItem* item = matches.takeFirst();

                item->setText(0, name_text);
                item->setText(1, stop_text);
                item->setText(2, print_text);
                item->setText(3, pass_text);
                item->setText(4, description_text);
                item->setText(5, "reused");
            }
        }

        // At this point, there are some new entries, some reused entries, and some unused ones.
        // Delete the unused ones. They are obsolete.
        QList<QTreeWidgetItem*> matches = signalsTreeWidget->findItems("unused", Qt::MatchExactly, 5);

        qDeleteAll(matches);

    }else{
        // Ignore others.
    }

    signalsTreeWidget->resizeColumnToContents(0);
    signalsTreeWidget->resizeColumnToContents(1);
    signalsTreeWidget->resizeColumnToContents(2);
    signalsTreeWidget->resizeColumnToContents(3);
    signalsTreeWidget->resizeColumnToContents(4);
    signalsTreeWidget->resizeColumnToContents(5);

    QApplication::restoreOverrideCursor();
}

void SeerSignalValuesBrowserWidget::handleStoppingPointReached () {

    // Don't do any work if the widget is hidden.
    if (isHidden()) {
        return;
    }

    if (_needsSignalNames) {

        emit refreshSignalNames();

        _needsSignalNames = false;
    }

    // refresh();
    // If a stopping point is reached, just refresh the values.
    // The signal names should already be there.
    emit refreshSignalValues("all");
}

void SeerSignalValuesBrowserWidget::handleSessionTerminated () {

    // Delete previous contents.
    signalsTreeWidget->clear();

    _needsSignalNames = true;
}

void SeerSignalValuesBrowserWidget::refresh () {

    _needsSignalNames = true;

    emit refreshSignalNames();
    emit refreshSignalValues("all");
}

void SeerSignalValuesBrowserWidget::handleItemDoubleClicked (QTreeWidgetItem* item, int column) {

    Q_UNUSED(column);

    _editItem(item);
}

void SeerSignalValuesBrowserWidget::handleItemEntered (QTreeWidgetItem* item, int column) {

    Q_UNUSED(column);

    item->setToolTip(0, item->text(0) + " : " + item->text(4));

    for (int i=1; i<signalsTreeWidget->columnCount(); i++) { // Copy tooltip to other columns.
        item->setToolTip(i, item->toolTip(0));
    }
}

void SeerSignalValuesBrowserWidget::handleContextMenu (const QPoint& pos) {

    // Get the item at the cursor.
    QTreeWidgetItem* item = signalsTreeWidget->itemAt(pos);

    // Construct the menu.
    QMenu*   menu          = new QMenu("Options", this);
    QAction* editAction    = menu->addAction("Edit selected");
    QAction* copyAction    = menu->addAction("Copy selected");
    QAction* copyAllAction = menu->addAction("Copy all");

    // If no selected item, disable 'selected' copy but allow 'all'.
    if (item == 0) {
        editAction->setEnabled(false);
        copyAction->setEnabled(false);
    }

    // Execute the menu. Return if nothing.
    QAction* action = menu->exec(signalsTreeWidget->mapToGlobal(pos));

    if (action == 0) {
        return;
    }

    // Do signal edit.
    if (action == editAction) {
        _editItem(item);
    }

    // Get selected tree items.
    QList<QTreeWidgetItem*> items;

    // Get list of 'select' items.
    if (action == copyAction) {
        items = signalsTreeWidget->selectedItems();
    }

    // Get list of 'all' items.
    if (action == copyAllAction) {
        items = signalsTreeWidget->findItems(QString("*"), Qt::MatchWrap | Qt::MatchWildcard);
    }

    // Populate the clipboard.
    if (items.size() == 0) {
        return;
    }

    QClipboard* clipboard = QGuiApplication::clipboard();

    QString text;

    text += QString("name") + " : " + "stop" + " " + "print" + " " + "pass" + '\n';

    for (int i=0; i<items.size(); i++) {

        if (items[i]->isHidden() == true) {
            continue;
        }

        text += items[i]->text(0) + " : " + items[i]->text(1) + " " + items[i]->text(2) + " " + items[i] ->text(3) + '\n';
    }

    clipboard->setText(text, QClipboard::Clipboard);
    clipboard->setText(text, QClipboard::Selection);
}

void SeerSignalValuesBrowserWidget::handleNewProfile () {

    if (signalsTreeWidget->topLevelItemCount() == 0) {
        QMessageBox::warning(this, "Seer", "When creating a profile, Seer needs to be debugging a program.", QMessageBox::Ok);
        return;
    }

    // Build a list of signals.
    QStringList   signalNames;
    QVector<bool> signalEnabled;

    for (int i=0; i<signalsTreeWidget->topLevelItemCount(); i++) {

        QTreeWidgetItem* item = signalsTreeWidget->topLevelItem(i);

        signalNames.push_back(item->text(0));
        signalEnabled.push_back(true);
    }

    // Bring up the signal profile dialog.
    SeerSignalProfileDialog dlg(this);

    dlg.setSignals(signalNames, signalEnabled);

    if (dlg.exec()) {

        QString profileName = dlg.profileName();

        if (profileName == "") {
            return;
        }

        signalNames   = dlg.signalNames();
        signalEnabled = dlg.signalEnabled();

        signalProfileComboBox->addItem(profileName);
        signalProfileComboBox->setCurrentText(profileName);

        writeProfileSettings(profileName, signalNames, signalEnabled);
        writeSettings();

        // Use them.
        _signalNames   = signalNames;
        _signalEnabled = signalEnabled;

        handleShowHideSignals();
    }
}

void SeerSignalValuesBrowserWidget::handleModifyProfile () {

    QString profileName = signalProfileComboBox->currentText();

    if (profileName == "allsignals") {
        QMessageBox::warning(this, "Seer", "The profile 'allsignals' is reserved.\n\nIt can't be modified.", QMessageBox::Ok);
        return;
    }

    // Build a list of signals.
    QStringList   signalNames   = _signalNames;
    QVector<bool> signalEnabled = _signalEnabled;

    // Bring up the signal profile dialog.
    SeerSignalProfileDialog dlg(this);

    dlg.setProfileName(profileName);
    dlg.setSignals(signalNames, signalEnabled);

    if (dlg.exec()) {

        // Get the (potenially new) profile name.
        QString newProfileName = dlg.profileName();

        if (newProfileName == "") {
            return;
        }

        signalNames   = dlg.signalNames();
        signalEnabled = dlg.signalEnabled();

        // If it's a new name, add it to the list and switch to it.
        if (newProfileName != profileName) {
            signalProfileComboBox->addItem(newProfileName);
            signalProfileComboBox->setCurrentText(newProfileName);
        }

        // Write out the setting changes.
        writeProfileSettings(newProfileName, signalNames, signalEnabled);
        writeSettings();

        // Use them.
        _signalNames   = signalNames;
        _signalEnabled = signalEnabled;

        handleShowHideSignals();
    }
}

void SeerSignalValuesBrowserWidget::handleDeleteProfile () {

    int index = signalProfileComboBox->currentIndex();

    if (index < 0) {
        return;
    }

    QString profileName = signalProfileComboBox->itemText(index);

    if (profileName == "") {
        return;
    }

    if (profileName == "allsignals") {
        QMessageBox::warning(this, "Seer", "The profile 'allsignals' is reserved.\n\nIt can't be deleted.", QMessageBox::Ok);
        return;
    }

    QMessageBox::StandardButton ans = QMessageBox::question(this, "Seer", "Delete profile '" + profileName + "'?");

    if (ans != QMessageBox::Yes) {
        return;
    }

    // Delete the profile from the settings.
    deleteProfileSettings(profileName);

    // Delete the profile from the list of profiles.
    signalProfileComboBox->removeItem(index);

    // Write the current settings.
    writeSettings();

    // Switch to the next profile in the list of profiles.
    index = signalProfileComboBox->currentIndex();

    if (index < 0) {
        return;
    }

    handleProfileChanged (index);
}

void SeerSignalValuesBrowserWidget::handleShowHideSignals () {

    // If the list is empty, show all.
    if (_signalNames.count() == 0 || _signalEnabled.count() == 0) {

        for (int i=0; i<signalsTreeWidget->topLevelItemCount(); i++) {

            QTreeWidgetItem* topItem = signalsTreeWidget->topLevelItem(i);
            topItem->setHidden(false);
        }

        return;
    }

    // There is a list. Hide all, then show which ones are enabled.
    for (int i=0; i<signalsTreeWidget->topLevelItemCount(); i++) {

        QTreeWidgetItem* topItem = signalsTreeWidget->topLevelItem(i);
        topItem->setHidden(true);
    }

    for (int i=0; i<_signalNames.count(); i++) {

        if (_signalEnabled[i] == false) {
            continue;
        }

        QString signalName = _signalNames[i];

        QList<QTreeWidgetItem*> matches = signalsTreeWidget->findItems(signalName, Qt::MatchExactly, 0);

        for (int i=0; i<matches.size(); i++) {
            matches[i]->setHidden(false);
        }
    }
}

void SeerSignalValuesBrowserWidget::handleProfileChanged (int index) {

    // Get the profile.
    QString profileName = signalProfileComboBox->itemText(index);

    // Build a list of signals.
    QStringList   signalNames;
    QVector<bool> signalEnabled;

    bool f = readProfileSettings(profileName, signalNames, signalEnabled);

    if (f == false) {
        return;
    }

    // Use them.
    _signalNames   = signalNames;
    _signalEnabled = signalEnabled;

    handleShowHideSignals();
    writeSettings();
}

void SeerSignalValuesBrowserWidget::writeSettings () {

    QSettings settings;

    QStringList signalprofiles;

    for (int i=0; i<signalProfileComboBox->count(); i++) {
        if (signalProfileComboBox->itemText(i) == "allsignals") {
            continue;
        }
        signalprofiles.push_back(signalProfileComboBox->itemText(i));
    }

    settings.beginGroup("signalwindow"); {
        settings.setValue("signalprofile",   signalProfileComboBox->currentText());
        settings.setValue("signalprofiles",  signalprofiles);
    } settings.endGroup();
}

void SeerSignalValuesBrowserWidget::readSettings () {

    QSettings settings;

    settings.beginGroup("signalwindow"); {

        QString     signalProfileName = settings.value("signalprofile").toString();
        QStringList signalProfiles    = settings.value("signalprofiles").toStringList();

        signalProfileComboBox->addItems(signalProfiles);
        signalProfileComboBox->setCurrentText(signalProfileName);

    } settings.endGroup();
}

void SeerSignalValuesBrowserWidget::writeProfileSettings (const QString& profileName, const QStringList& signalNames, const QVector<bool>& signalEnabled) {

    QSettings   settings;
    QStringList signalEnabledFlags;

    for (int i=0; i<signalEnabled.count(); i++) {
        signalEnabledFlags.append(signalEnabled[i] == true ? "T" : "F");
    }

    settings.beginGroup("signalprofile_" + profileName); {
        settings.setValue("signalnames",   signalNames);
        settings.setValue("signalenabled", signalEnabledFlags);
    } settings.endGroup();
}

bool SeerSignalValuesBrowserWidget::readProfileSettings (const QString& profileName, QStringList& signalNames, QVector<bool>& signalEnabled) {

    if (profileName == "" || profileName == "allsignals") {

        signalNames.resize(0);
        signalEnabled.resize(0);

        return true;
    }

    QSettings settings;

    settings.beginGroup("signalprofile_" + profileName); {

        QVariant    signalNamesVariant   = settings.value("signalnames");
        QVariant    signalEnabledVariant = settings.value("signalenabled");
        QStringList signalEnabledFlags;

        // Not found? Return with error.
        if (signalNamesVariant == QVariant() || signalEnabledVariant == QVariant()) {
            return false;
        }

        // Okay, we found it. Populate the containters.
        signalNames        = signalNamesVariant.toStringList();
        signalEnabledFlags = signalEnabledVariant.toStringList();

        signalEnabled.resize(0);
        for (int i=0; i<signalEnabledFlags.count(); i++) {
            signalEnabled.append(signalEnabledFlags[i] == "T" ? true : false);
        }

    } settings.endGroup();

    return true;
}

void SeerSignalValuesBrowserWidget::deleteProfileSettings (const QString& profileName) {

    QSettings settings;

    settings.beginGroup("signalprofile_" + profileName); {
        settings.remove(""); //removes the group, and all it keys
    } settings.endGroup();
}

void SeerSignalValuesBrowserWidget::showEvent (QShowEvent* event) {

    QWidget::showEvent(event);

    refresh();
}

void SeerSignalValuesBrowserWidget::_editItem (QTreeWidgetItem* item) {

        // Bring up the register edit dialog.
        SeerSignalEditValueDialog dlg(this);

        dlg.set(item->text(0), item->text(1), item->text(2), item->text(3), item->text(4));

        int ret = dlg.exec();

        if (ret == 0) {
            return;
        }

        // The register name could be changed, as well as the value.
        QString name  = dlg.nameText();
        QString stop  = dlg.stopText();
        QString print = dlg.printText();
        QString pass  = dlg.passText();

        if (name == "" || stop == "" || print == "" || pass == "") {
            return;
        }

        // Emit the signal to change the register to the new value.
        emit setSignalValue(name, stop, print, pass);

        return;
}

