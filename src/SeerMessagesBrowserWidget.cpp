#include "SeerMessagesBrowserWidget.h"
#include <QtWidgets/QTreeWidget>
#include <QtWidgets/QTreeWidgetItemIterator>
#include <QtWidgets/QMessageBox>
#include <QtCore/QTime>
#include <QtCore/QDebug>

SeerMessagesBrowserWidget::SeerMessagesBrowserWidget (QWidget* parent) : QWidget(parent) {

    // Construct the UI.
    setupUi(this);

    // Setup the widgets
    QString style = "QTreeWidget::item:!selected "          // Items in tree widget will have a border.
                    "{ "
                       "border: 1px solid gainsboro; "
                       "border-left: none; "
                       "border-top: none; "
                    "}"
                    "QTreeWidget::item:selected {}";

    messagesTreeWidget->setRootIsDecorated(false);
    messagesTreeWidget->setStyleSheet(style);
    messagesTreeWidget->setSortingEnabled(false);
    messagesTreeWidget->resizeColumnToContents(0); // timestamp
    messagesTreeWidget->resizeColumnToContents(1); // message type icon
    messagesTreeWidget->resizeColumnToContents(2); // message

    // Get icons.
    _informationIcon = QIcon(":/seer/resources/RelaxLightIcons/data-information.svg");
    _warningIcon     = QIcon(":/seer/resources/RelaxLightIcons/data-warning.svg");
    _criticalIcon    = QIcon(":/seer/resources/RelaxLightIcons/data-error.svg");
    _questionIcon    = QIcon(":/seer/resources/RelaxLightIcons/dialog-question.svg");


    // Connect things.
    QObject::connect(deleteMessagesToolButton,   &QToolButton::clicked,         this,  &SeerMessagesBrowserWidget::handleDeleteToolButton);

    // Clear messages
    clearMessages();
}

SeerMessagesBrowserWidget::~SeerMessagesBrowserWidget () {
}

void SeerMessagesBrowserWidget::addMessage (const QString& message, QMessageBox::Icon messageType) {

    // Give this dialog the focus.
    //setFocus(Qt::OtherFocusReason);

    // Show messages any time a messaged is added.
    emit showMessages();

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
    messagesTreeWidget->addTopLevelItem(item);

    // Re-adjust column sizes.
    messagesTreeWidget->resizeColumnToContents(0);
    messagesTreeWidget->resizeColumnToContents(1);
    messagesTreeWidget->resizeColumnToContents(2);

    // Scroll to the bottom.
    QTreeWidgetItem* lastItem = messagesTreeWidget->topLevelItem(messagesTreeWidget->topLevelItemCount()-1);
    if (lastItem) {
        messagesTreeWidget->scrollToItem(lastItem);
        messagesTreeWidget->clearSelection();
        lastItem->setSelected(true);
    }
}

void SeerMessagesBrowserWidget::clearMessages () {

    messagesTreeWidget->clear();
}

void SeerMessagesBrowserWidget::handleDeleteToolButton () {

    // Delete all messages.
    clearMessages();
}

