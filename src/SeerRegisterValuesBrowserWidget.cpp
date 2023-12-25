#include "SeerRegisterValuesBrowserWidget.h"
#include "SeerRegisterEditValueDialog.h"
#include "SeerRegisterProfileDialog.h"
#include "SeerRegisterTreeWidgetItem.h"
#include "SeerUtl.h"
#include "QEditDelegate.h"
#include <QtWidgets/QTreeWidget>
#include <QtWidgets/QTreeWidgetItemIterator>
#include <QtWidgets/QApplication>
#include <QtWidgets/QMenu>
#include <QtWidgets/QMessageBox>
#include <QtGui/QFontDatabase>
#include <QtGui/QClipboard>
#include <QtCore/QSettings>
#include <QtCore/QVector>
#include <QtCore/QByteArray>
#include <QtCore/QDebug>


SeerRegisterValuesBrowserWidget::SeerRegisterValuesBrowserWidget (QWidget* parent) : QWidget(parent) {

    _newProfileAction    = 0;
    _deleteProfileAction = 0;

    // Construct the UI.
    setupUi(this);

    // Setup the widgets
    registersTreeWidget->setMouseTracking(true);
    registersTreeWidget->sortByColumn(0, Qt::AscendingOrder);
    registersTreeWidget->setSortingEnabled(true);  // We can sort on columns.
    registersTreeWidget->setContextMenuPolicy(Qt::CustomContextMenu);
    registersTreeWidget->resizeColumnToContents(0); // number
    registersTreeWidget->resizeColumnToContents(1); // name
    registersTreeWidget->resizeColumnToContents(2); // value
    registersTreeWidget->resizeColumnToContents(3); // used
    registersTreeWidget->setColumnHidden(3, true); // Hide the 'used' column.
    registersTreeWidget->clear();

    registerFormatComboBox->addItem("Natural", QVariant("N"));
    registerFormatComboBox->addItem("Hex",     QVariant("x"));
    registerFormatComboBox->addItem("Octal",   QVariant("o"));
    registerFormatComboBox->addItem("Binary",  QVariant("t"));
    registerFormatComboBox->addItem("Decimal", QVariant("d"));
    registerFormatComboBox->addItem("Raw",     QVariant("r"));

    registerProfileComboBox->addItem("allregisters");

    _needsRegisterNames = true;

    // Create edit delegate.
    QAllowEditDelegate* editDelegate = new QAllowEditDelegate(this);

    registersTreeWidget->setItemDelegateForColumn(0, new QNoEditDelegate(this));
    registersTreeWidget->setItemDelegateForColumn(1, new QNoEditDelegate(this));
    registersTreeWidget->setItemDelegateForColumn(2, editDelegate);
    registersTreeWidget->setItemDelegateForColumn(3, new QNoEditDelegate(this));


    // Preference menu.
    QMenu* menu = new QMenu();

    _newProfileAction    = menu->addAction("New profile");
    _modifyProfileAction = menu->addAction("Modify current profile");
    _deleteProfileAction = menu->addAction("Delete current profile");

    preferencesToolButton->setMenu(menu);
    preferencesToolButton->setPopupMode(QToolButton::InstantPopup);

    // Connect things.
    QObject::connect(registersTreeWidget,               &QTreeWidget::itemEntered,                                 this, &SeerRegisterValuesBrowserWidget::handleItemEntered);
    QObject::connect(registersTreeWidget,               &QTreeWidget::customContextMenuRequested,                  this, &SeerRegisterValuesBrowserWidget::handleContextMenu);
    QObject::connect(editDelegate,                      &QAllowEditDelegate::editingFinished,                      this, &SeerRegisterValuesBrowserWidget::handleIndexEditingFinished);
    QObject::connect(registerFormatComboBox,            QOverload<int>::of(&QComboBox::currentIndexChanged),       this, &SeerRegisterValuesBrowserWidget::handleFormatChanged);
    QObject::connect(registersTreeWidget->header(),     &QHeaderView::sectionClicked,                              this, &SeerRegisterValuesBrowserWidget::handleColumnSelected);
    QObject::connect(_newProfileAction,                 &QAction::triggered,                                       this, &SeerRegisterValuesBrowserWidget::handleNewProfile);
    QObject::connect(_modifyProfileAction,              &QAction::triggered,                                       this, &SeerRegisterValuesBrowserWidget::handleModifyProfile);
    QObject::connect(_deleteProfileAction,              &QAction::triggered,                                       this, &SeerRegisterValuesBrowserWidget::handleDeleteProfile);
    QObject::connect(registerProfileComboBox,           QOverload<int>::of(&QComboBox::currentIndexChanged),       this, &SeerRegisterValuesBrowserWidget::handleProfileChanged);

    // Restore settings.
    readSettings();
}

SeerRegisterValuesBrowserWidget::~SeerRegisterValuesBrowserWidget () {
}

void SeerRegisterValuesBrowserWidget::setRegisterFormat (QString fmt) {

    registerFormatComboBox->setCurrentText(fmt);
}

void SeerRegisterValuesBrowserWidget::handleText (const QString& text) {

    // Don't do any work if the widget is hidden.
    if (isHidden()) {
        return;
    }

    QApplication::setOverrideCursor(Qt::BusyCursor);

    if (text.startsWith("^done,register-names=[") && text.endsWith("]")) {

        // "^done,register-names=[\"rax\",\"rbx\",\"rcx\",\"rdx\",\"rsi\",\"rdi\",\"rbp\",\"rsp\",
        //                        \"r8\",\"r9\",\"r10\",\"r11\",\"r12\",\"r13\",\"r14\",\"r15\",
        //                        \"rip\",\"eflags\",\"cs\",\"ss\",\"ds\",\"es\",\"fs\",\"gs\",

        // This recreates the tree.
        registersTreeWidget->clear();

        QString newtext = Seer::filterEscapes(text); // Filter escaped characters.

        QString frame_text = Seer::parseFirst(newtext, "register-names=", '[', ']', false);

        QStringList name_list = Seer::parse(frame_text, "", '"', '"', false);

        int i = 0;
        for ( const auto& name_text : name_list  ) {

            // XXX Commenting this out will get extra registers.
            // XXX Are they valid?
            if (name_text == "") {
                continue;
            }

            QTreeWidgetItem* topItem = new SeerRegisterTreeWidgetItem;
            topItem->setFlags(topItem->flags() | Qt::ItemIsEditable);
            topItem->setText(0, QString::number(i));
            topItem->setText(1, name_text);
            topItem->setText(2, "");
            topItem->setText(3, "new");

            topItem->setFont(2, QFontDatabase::systemFont(QFontDatabase::FixedFont));

            registersTreeWidget->addTopLevelItem(topItem);

            i++;
        }

        handleShowHideRegisters();

        _needsRegisterNames = false;

    }else if (text.startsWith("^done,register-values=[") && text.endsWith("]")) {

        // Mark each entry initially as "unused".
        // Later, some will be marked as "reused" or "new". Then the "unused" ones will
        // be deleted.
        QTreeWidgetItemIterator it(registersTreeWidget);
        while (*it) {
            (*it)->setText(3, "unused");
            ++it;
        }

        // "^done,register-values=[{number=\"0\",value=\"0x4005e7\"},{number=\"1\",value=\"0x0\"},{number=\"2\",value=\"0x100\"},
        //                    {number=\"3\",value=\"0x7fffffffd548\"},{number=\"4\",value=\"0x7fffffffd538\"},{number=\"5\",value=\"0x1\"},...
        //                    {number=\"205\",value=\"0x0\"},{number=\"206\",value=\"0x0\"}]"

        QString newtext = Seer::filterEscapes(text); // Filter escaped characters.

        QString frame_text = Seer::parseFirst(newtext, "register-values=", '[', ']', false);

        QStringList registers_list = Seer::parse(frame_text, "", '{', '}', false);

        for ( const auto& register_text : registers_list  ) {

            QString number_text = Seer::parseFirst(register_text, "number=",  '"', '"', false);
            QString value_text  = Seer::parseFirst(register_text, "value=",   '"', '"', false);

            // Instead of creating a new tree each time, we will reuse existing items, if they are there.
            // This allows the expanded items to remain expanded.
            QList<QTreeWidgetItem*> matches = registersTreeWidget->findItems(number_text, Qt::MatchExactly, 0);

            // Found a match. Reuse it. Set the value.
            // If no matches, do not add this entry.
            if (matches.size() > 0) {

                QTreeWidgetItem* item = matches.takeFirst();

                bool isDifferent = false;

                // Flag it as different of the new value is different than the old version _AND_
                // the old one isn't "" (like after a refresh).
                if (item->text(2) != "" && item->text(2) != value_text) {
                    isDifferent = true;
                }

                item->setText(2, value_text);
                item->setText(3, "reused");

                QFont f1 = item->font(1);
                QFont f2 = item->font(2);

                f1.setBold(isDifferent);
                f2.setBold(isDifferent);

                item->setFont(1, f1);
                item->setFont(2, f2);
            }
        }

        // At this point, there are some new entries, some reused entries, and some unused ones.
        // Delete the unused ones. They are obsolete.
        QList<QTreeWidgetItem*> matches = registersTreeWidget->findItems("unused", Qt::MatchExactly, 3);

        qDeleteAll(matches);

    }else if (text.startsWith("^error,msg=\"No registers.\"")) {
        registersTreeWidget->clear();

        _needsRegisterNames = true;

    }else{
        // Ignore others.
    }

    registersTreeWidget->resizeColumnToContents(0);
    registersTreeWidget->resizeColumnToContents(1);
    registersTreeWidget->resizeColumnToContents(2);
    registersTreeWidget->resizeColumnToContents(3);

    QApplication::restoreOverrideCursor();
}

void SeerRegisterValuesBrowserWidget::handleStoppingPointReached () {

    // Don't do any work if the widget is hidden.
    if (isHidden()) {
        return;
    }

    if (_needsRegisterNames) {

        emit refreshRegisterNames();

        _needsRegisterNames = false;
    }

    // Get the format.
    QString fmt = registerFormatComboBox->currentData().toString();

    // refresh();
    // If a stopping point is reached, just refresh the values.
    // The register names should already be there.
    emit refreshRegisterValues(fmt);
}

void SeerRegisterValuesBrowserWidget::refresh () {

    // Force new names.
    _needsRegisterNames = true;

    // Get the format.
    QString fmt = registerFormatComboBox->currentData().toString();

    emit refreshRegisterNames();
    emit refreshRegisterValues(fmt);
}

void SeerRegisterValuesBrowserWidget::handleItemEntered (QTreeWidgetItem* item, int column) {

    Q_UNUSED(column);

    item->setToolTip(0, item->text(1) + " : " + item->text(2));

    for (int i=1; i<registersTreeWidget->columnCount(); i++) { // Copy tooltip to other columns.
        item->setToolTip(i, item->toolTip(0));
    }
}

void SeerRegisterValuesBrowserWidget::handleContextMenu (const QPoint& pos) {

    // Get the item at the cursor.
    QTreeWidgetItem* item = registersTreeWidget->itemAt(pos);

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
    QAction* action = menu->exec(registersTreeWidget->mapToGlobal(pos));

    if (action == 0) {
        return;
    }

    // Do register edit.
    if (action == editAction) {

        // Bring up the register edit dialog.
        SeerRegisterEditValueDialog dlg(this);

        dlg.set(item->text(1), item->text(2));

        int ret = dlg.exec();

        if (ret == 0) {
            return;
        }

        // The register name could be changed, as well as the value.
        QString name  = dlg.nameText();
        QString value = dlg.valueText();

        if (name == "") {
            return;
        }

        if (value == "") {
            return;
        }

        // Get the format.
        QString fmt = registerFormatComboBox->currentData().toString();

        // Emit the signal to change the register to the new value.
        emit setRegisterValue(fmt, name, value);

        return;
    }

    // Get selected tree items.
    QList<QTreeWidgetItem*> items;

    // Get list of 'select' items.
    if (action == copyAction) {
        items = registersTreeWidget->selectedItems();
    }

    // Get list of 'all' items.
    if (action == copyAllAction) {
        items = registersTreeWidget->findItems(QString("*"), Qt::MatchWrap | Qt::MatchWildcard);
    }

    // Populate the clipboard.
    if (items.size() == 0) {
        return;
    }

    QClipboard* clipboard = QGuiApplication::clipboard();

    QString text;

    for (int i=0; i<items.size(); i++) {

        if (i != 0) {
            text += '\n';
        }

        text += items[i]->text(1) + ":" + items[i]->text(2);
    }

    clipboard->setText(text, QClipboard::Clipboard);
    clipboard->setText(text, QClipboard::Selection);
}

void SeerRegisterValuesBrowserWidget::handleIndexEditingFinished  (const QModelIndex& index) {

    QTreeWidgetItem* item = registersTreeWidget->getItemFromIndex(index);

    if (item == 0) {
        return;
    }

    // Get the format.
    QString fmt = registerFormatComboBox->currentData().toString();

    // Get the new value;
    QString value = item->text(2);

    // Emit the signal to change the register to the new value.
    emit setRegisterValue(fmt, item->text(1), value);
}

void SeerRegisterValuesBrowserWidget::handleFormatChanged (int index) {

    // Get the format.
    QString fmt = registerFormatComboBox->itemData(index).toString();

    // Refresh the register values.
    emit refreshRegisterValues(fmt);
}

void SeerRegisterValuesBrowserWidget::handleColumnSelected (int logicalIndex) {

    Q_UNUSED(logicalIndex);

    writeSettings();
}

void SeerRegisterValuesBrowserWidget::handleNewProfile () {

    if (registersTreeWidget->topLevelItemCount() == 0) {
        QMessageBox::warning(this, "Seer", "When creating a profile, Seer needs to be debugging a program.", QMessageBox::Ok);
        return;
    }

    // Build a list of registers.
    QStringList   registerNames;
    QVector<bool> registerEnabled;

    for (int i=0; i<registersTreeWidget->topLevelItemCount(); i++) {

        QTreeWidgetItem* item = registersTreeWidget->topLevelItem(i);

        registerNames.push_back(item->text(1));
        registerEnabled.push_back(true);
    }

    // Bring up the register profile dialog.
    SeerRegisterProfileDialog dlg(this);

    dlg.setRegisters(registerNames, registerEnabled);

    if (dlg.exec()) {

        QString profileName = dlg.profileName();

        if (profileName == "") {
            return;
        }

        registerNames   = dlg.registerNames();
        registerEnabled = dlg.registerEnabled();

        registerProfileComboBox->addItem(profileName);
        registerProfileComboBox->setCurrentText(profileName);

        writeProfileSettings(profileName, registerNames, registerEnabled);
        writeSettings();

        // Use them.
        _registerNames   = registerNames;
        _registerEnabled = registerEnabled;

        handleShowHideRegisters();
    }
}

void SeerRegisterValuesBrowserWidget::handleModifyProfile () {

    QString profileName = registerProfileComboBox->currentText();

    if (profileName == "allregisters") {
        QMessageBox::warning(this, "Seer", "The profile 'allregisters' is reserved.\n\nIt can't be modified.", QMessageBox::Ok);
        return;
    }

    // Build a list of registers.
    QStringList   registerNames   = _registerNames;
    QVector<bool> registerEnabled = _registerEnabled;

    // Bring up the register profile dialog.
    SeerRegisterProfileDialog dlg(this);

    dlg.setProfileName(profileName);
    dlg.setRegisters(registerNames, registerEnabled);

    if (dlg.exec()) {

        // Get the (potenially new) profile name.
        QString newProfileName = dlg.profileName();

        if (newProfileName == "") {
            return;
        }

        registerNames   = dlg.registerNames();
        registerEnabled = dlg.registerEnabled();

        // If it's a new name, add it to the list and switch to it.
        if (newProfileName != profileName) {
            registerProfileComboBox->addItem(newProfileName);
            registerProfileComboBox->setCurrentText(newProfileName);
        }

        // Write out the setting changes.
        writeProfileSettings(newProfileName, registerNames, registerEnabled);
        writeSettings();

        // Use them.
        _registerNames   = registerNames;
        _registerEnabled = registerEnabled;

        handleShowHideRegisters();
    }
}

void SeerRegisterValuesBrowserWidget::handleDeleteProfile () {

    int index = registerProfileComboBox->currentIndex();

    if (index < 0) {
        return;
    }

    QString profileName = registerProfileComboBox->itemText(index);

    if (profileName == "") {
        return;
    }

    if (profileName == "allregisters") {
        QMessageBox::warning(this, "Seer", "The profile 'allregisters' is reserved.\n\nIt can't be deleted.", QMessageBox::Ok);
        return;
    }

    QMessageBox::StandardButton ans = QMessageBox::question(this, "Seer", "Delete profile '" + profileName + "'?");

    if (ans != QMessageBox::Yes) {
        return;
    }

    // Delete the profile from the settings.
    deleteProfileSettings(profileName);

    // Delete the profile from the list of profiles.
    registerProfileComboBox->removeItem(index);

    // Write the current settings.
    writeSettings();

    // Switch to the next profile in the list of profiles.
    index = registerProfileComboBox->currentIndex();

    if (index < 0) {
        return;
    }

    handleProfileChanged (index);
}

void SeerRegisterValuesBrowserWidget::handleShowHideRegisters () {

    // If the list is empty, show all.
    if (_registerNames.count() == 0 || _registerEnabled.count() == 0) {

        for (int i=0; i<registersTreeWidget->topLevelItemCount(); i++) {

            QTreeWidgetItem* topItem = registersTreeWidget->topLevelItem(i);
            topItem->setHidden(false);
        }

        return;
    }

    // There is a list. Hide all, then show which ones are enabled.
    for (int i=0; i<registersTreeWidget->topLevelItemCount(); i++) {

        QTreeWidgetItem* topItem = registersTreeWidget->topLevelItem(i);
        topItem->setHidden(true);
    }

    for (int i=0; i<_registerNames.count(); i++) {

        if (_registerEnabled[i] == false) {
            continue;
        }

        QString registerName = _registerNames[i];

        QList<QTreeWidgetItem*> matches = registersTreeWidget->findItems(registerName, Qt::MatchExactly, 1);

        for (int i=0; i<matches.size(); i++) {
            matches[i]->setHidden(false);
        }
    }
}

void SeerRegisterValuesBrowserWidget::handleProfileChanged (int index) {

    // Get the profile.
    QString profileName = registerProfileComboBox->itemText(index);

    // Build a list of registers.
    QStringList   registerNames;
    QVector<bool> registerEnabled;

    bool f = readProfileSettings(profileName, registerNames, registerEnabled);

    if (f == false) {
        return;
    }

    // Use them.
    _registerNames   = registerNames;
    _registerEnabled = registerEnabled;

    handleShowHideRegisters();
    writeSettings();
}

void SeerRegisterValuesBrowserWidget::writeSettings () {

    QSettings settings;

    QStringList registerprofiles;

    for (int i=0; i<registerProfileComboBox->count(); i++) {
        if (registerProfileComboBox->itemText(i) == "allregisters") {
            continue;
        }
        registerprofiles.push_back(registerProfileComboBox->itemText(i));
    }

    settings.beginGroup("registerwindow"); {
        settings.setValue("sortcolumn",        registersTreeWidget->header()->sortIndicatorSection());
        settings.setValue("sortorder",         registersTreeWidget->header()->sortIndicatorOrder());
        settings.setValue("registerprofile",   registerProfileComboBox->currentText());
        settings.setValue("registerprofiles",  registerprofiles);
    } settings.endGroup();
}

void SeerRegisterValuesBrowserWidget::readSettings () {

    QSettings settings;

    settings.beginGroup("registerwindow"); {

        int column = settings.value("sortcolumn", 0).toInt();
        int order  = settings.value("sortorder",  Qt::AscendingOrder).toInt();

        registersTreeWidget->header()->setSortIndicator(column, (Qt::SortOrder)order);

        QString     registerProfileName = settings.value("registerprofile").toString();
        QStringList registerProfiles    = settings.value("registerprofiles").toStringList();

        registerProfileComboBox->addItems(registerProfiles);
        registerProfileComboBox->setCurrentText(registerProfileName);

    } settings.endGroup();
}

void SeerRegisterValuesBrowserWidget::writeProfileSettings (const QString& profileName, const QStringList& registerNames, const QVector<bool>& registerEnabled) {

    QSettings   settings;
    QStringList registerEnabledFlags;

    for (int i=0; i<registerEnabled.count(); i++) {
        registerEnabledFlags.append(registerEnabled[i] == true ? "T" : "F");
    }

    settings.beginGroup("registerprofile_" + profileName); {
        settings.setValue("registernames",   registerNames);
        settings.setValue("registerenabled", registerEnabledFlags);
    } settings.endGroup();
}

bool SeerRegisterValuesBrowserWidget::readProfileSettings (const QString& profileName, QStringList& registerNames, QVector<bool>& registerEnabled) {

    if (profileName == "" || profileName == "allregisters") {

        registerNames.resize(0);
        registerEnabled.resize(0);

        return true;
    }

    QSettings settings;

    settings.beginGroup("registerprofile_" + profileName); {

        QVariant    registerNamesVariant   = settings.value("registernames");
        QVariant    registerEnabledVariant = settings.value("registerenabled");
        QStringList registerEnabledFlags;

        // Not found? Return with error.
        if (registerNamesVariant == QVariant() || registerEnabledVariant == QVariant()) {
            return false;
        }

        // Okay, we found it. Populate the containters.
        registerNames        = registerNamesVariant.toStringList();
        registerEnabledFlags = registerEnabledVariant.toStringList();

        registerEnabled.resize(0);
        for (int i=0; i<registerEnabledFlags.count(); i++) {
            registerEnabled.append(registerEnabledFlags[i] == "T" ? true : false);
        }

    } settings.endGroup();

    return true;
}

void SeerRegisterValuesBrowserWidget::deleteProfileSettings (const QString& profileName) {

    QSettings   settings;

    settings.beginGroup("registerprofile_" + profileName); {
        settings.remove(""); //removes the group, and all it keys
    } settings.endGroup();
}

void SeerRegisterValuesBrowserWidget::showEvent (QShowEvent* event) {

    QWidget::showEvent(event);

    refresh();
}

