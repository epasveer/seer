#include "SeerMessagesDialog.h"
#include <QtWidgets/QTreeWidget>
#include <QtWidgets/QTreeWidgetItem>
#include <QtWidgets/QDialogButtonBox>
#include <QtCore/QTime>
#include <QtCore/QSettings>
#include <QtCore/QDebug>


SeerMessagesDialog::SeerMessagesDialog (QWidget* parent) : QDialog(parent) {

    _closePushButton = 0;
    _clearPushButton = 0;

    // Construct the UI.
    setupUi(this);

    setWindowIcon(QIcon(":/seer/resources/seergdb_64x64.png"));
    setWindowTitle("Seer Execution Messages");

    // Setup the widgets.
    QString style = "QTreeWidget::item:!selected "          // Items in tree widget will have a border.
                    "{ "
                       "border: 1px solid gainsboro; "
                       "border-left: none; "
                       "border-top: none; "
                    "}"
                    "QTreeWidget::item:selected {}";

    messageTreeWidget->setRootIsDecorated(false);
    messageTreeWidget->setStyleSheet(style);
    messageTreeWidget->setSortingEnabled(false);
    messageTreeWidget->resizeColumnToContents(0); // timestamp
    messageTreeWidget->resizeColumnToContents(1); // message type icon
    messageTreeWidget->resizeColumnToContents(2); // message

    // Add a "CLEAR" button to the dialog's buttonbox.
    // Make sure the CLOSE has the default action still.
    QDialogButtonBox* buttonBox = findChild<QDialogButtonBox*>("buttonBox");

    if (buttonBox) {

        _clearPushButton = buttonBox->addButton("Clear", QDialogButtonBox::ActionRole);

        QObject::connect(_clearPushButton, &QPushButton::clicked,      this, &SeerMessagesDialog::clearMessages);

         _closePushButton = buttonBox->button(QDialogButtonBox::Close);
    }

    resetDefaultButton();

    // Get icons.
    _informationIcon = QIcon(":/seer/resources/RelaxLightIcons/data-information.svg");
    _warningIcon     = QIcon(":/seer/resources/RelaxLightIcons/data-warning.svg");
    _criticalIcon    = QIcon(":/seer/resources/RelaxLightIcons/data-error.svg");
    _questionIcon    = QIcon(":/seer/resources/RelaxLightIcons/dialog-question.svg");

    // Connect things.

    // Clear messages
    clearMessages();

    // Restore window settings.
    readSettings();

    // Hide right away.
    hide();
}

SeerMessagesDialog::~SeerMessagesDialog () {
}

void SeerMessagesDialog::addMessage (const QString& message, QMessageBox::Icon messageType) {

    // Give this dialog the focus.
    //setFocus(Qt::OtherFocusReason);

    // Show messages any time a messaged is added.
    showMessages();

    // Create an entry with our message.
    QTreeWidgetItem* item = new QTreeWidgetItem;
    item->setText(0, QTime::currentTime().toString(Qt::TextDate));

    switch (messageType) {
        case QMessageBox::NoIcon:
            item->setIcon(1, _noIcon);
            break;
        case QMessageBox::Information:
            item->setIcon(1, _informationIcon);
            break;
        case QMessageBox::Warning:
            item->setIcon(1, _warningIcon);
            break;
        case QMessageBox::Critical:
            item->setIcon(1, _criticalIcon);
            break;
        case QMessageBox::Question:
            item->setIcon(1, _questionIcon);
            break;
        default:
            item->setIcon(1, _noIcon);
            break;
    }

    item->setText(2, message);

    // Insert the entry (at the end).
    messageTreeWidget->addTopLevelItem(item);

    // Re-adjust column sizes.
    messageTreeWidget->resizeColumnToContents(0);
    messageTreeWidget->resizeColumnToContents(1);
    messageTreeWidget->resizeColumnToContents(2);

    // Scroll to the bottom.
    QTreeWidgetItem* lastItem = messageTreeWidget->topLevelItem(messageTreeWidget->topLevelItemCount()-1);
    if (lastItem) {
        messageTreeWidget->scrollToItem(lastItem);
        messageTreeWidget->clearSelection();
        lastItem->setSelected(true);
    }

    resetDefaultButton();
}

void SeerMessagesDialog::clearMessages () {

    messageTreeWidget->clear();

    resetDefaultButton();
}

void SeerMessagesDialog::showMessages () {

    // Show the dialog.
    show();

    resetDefaultButton();
}

void SeerMessagesDialog::writeSettings () {

    QSettings settings;

    settings.beginGroup("executionmessagesdialog"); {
        settings.setValue("size", size());
    }settings.endGroup();
}

void SeerMessagesDialog::readSettings () {

    QSettings settings;

    settings.beginGroup("executionmessagesdialog"); {
        resize(settings.value("size", QSize(425, 150)).toSize());
    } settings.endGroup();
}

void SeerMessagesDialog::resizeEvent (QResizeEvent* event) {

    // Write window settings.
    writeSettings();

    QDialog::resizeEvent(event);
}

void SeerMessagesDialog::resetDefaultButton () {

    if (_clearPushButton) {
        _clearPushButton->setDefault(false);
    }

    if (_closePushButton) {
        _closePushButton->setDefault(true);
    }
}

