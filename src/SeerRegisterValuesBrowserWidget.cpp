#include "SeerRegisterValuesBrowserWidget.h"
#include "SeerRegisterEditValueDialog.h"
#include "SeerUtl.h"
#include "QEditDelegate.h"
#include <QtWidgets/QTreeWidget>
#include <QtWidgets/QTreeWidgetItemIterator>
#include <QtWidgets/QApplication>
#include <QtWidgets/QMenu>
#include <QtGui/QFontDatabase>
#include <QtGui/QClipboard>
#include <QtCore/QDebug>

SeerRegisterValuesBrowserWidget::SeerRegisterValuesBrowserWidget (QWidget* parent) : QWidget(parent) {

    // Construct the UI.
    setupUi(this);

    // Setup the widgets
    registersTreeWidget->setMouseTracking(true);
    registersTreeWidget->setSortingEnabled(false);
    registersTreeWidget->setContextMenuPolicy(Qt::CustomContextMenu);
    registersTreeWidget->resizeColumnToContents(0); // number
    registersTreeWidget->resizeColumnToContents(1); // name
    registersTreeWidget->resizeColumnToContents(2); // value
    registersTreeWidget->resizeColumnToContents(3); // used
    registersTreeWidget->setColumnHidden(0, true); // Hide the 'number' column.
    registersTreeWidget->setColumnHidden(3, true); // Hide the 'used' column.
    registersTreeWidget->clear();

    registerFormatComboBox->addItem("Natural", QVariant("N"));
    registerFormatComboBox->addItem("Hex",     QVariant("x"));
    registerFormatComboBox->addItem("Octal",   QVariant("o"));
    registerFormatComboBox->addItem("Binary",  QVariant("t"));
    registerFormatComboBox->addItem("Decimal", QVariant("d"));
    registerFormatComboBox->addItem("Raw",     QVariant("r"));

    _needsRegisterNames = true;

    // Create edit delegate.
    QAllowEditDelegate* editDelegate = new QAllowEditDelegate(this);

    registersTreeWidget->setItemDelegateForColumn(0, new QNoEditDelegate(this));
    registersTreeWidget->setItemDelegateForColumn(1, new QNoEditDelegate(this));
    registersTreeWidget->setItemDelegateForColumn(2, editDelegate);
    registersTreeWidget->setItemDelegateForColumn(3, new QNoEditDelegate(this));

    // Connect things.
    QObject::connect(registersTreeWidget,    &QTreeWidget::itemEntered,                                 this, &SeerRegisterValuesBrowserWidget::handleItemEntered);
    QObject::connect(registersTreeWidget,    &QTreeWidget::customContextMenuRequested,                  this, &SeerRegisterValuesBrowserWidget::handleContextMenu);
    QObject::connect(editDelegate,           &QAllowEditDelegate::editingFinished,                      this, &SeerRegisterValuesBrowserWidget::handleIndexEditingFinished);
    QObject::connect(registerFormatComboBox, QOverload<int>::of(&QComboBox::currentIndexChanged),       this, &SeerRegisterValuesBrowserWidget::handleFormatChanged);
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

        //qDebug() << frame_text;

        QStringList name_list = Seer::parse(frame_text, "", '"', '"', false);

        int i = 0;
        for ( const auto& name_text : name_list  ) {

            if (name_text == "") {
                continue;
            }

            QTreeWidgetItem* topItem = new QTreeWidgetItem;
            topItem->setFlags(topItem->flags() | Qt::ItemIsEditable);
            topItem->setText(0, QString::number(i));
            topItem->setText(1, name_text);
            topItem->setText(2, "");
            topItem->setText(3, "new");

            topItem->setFont(2, QFontDatabase::systemFont(QFontDatabase::FixedFont));

            registersTreeWidget->addTopLevelItem(topItem);

            i++;
        }

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

        //qDebug() << text;

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
    _needsRegisterNames = false;

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

void SeerRegisterValuesBrowserWidget::showEvent (QShowEvent* event) {

    QWidget::showEvent(event);

    refresh();
}

