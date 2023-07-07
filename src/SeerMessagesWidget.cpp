#include "SeerMessagesWidget.h"
#include <QtWidgets/QTreeWidget>
#include <QtWidgets/QTreeWidgetItem>
#include <QtWidgets/QApplication>
#include <QtCore/QTime>
#include <QtCore/QSettings>
#include <QtCore/QDebug>


SeerMessagesWidget::SeerMessagesWidget (QWidget* parent) : QWidget(parent) {

    // Construct the UI.
    setupUi(this);

    Qt::WindowFlags flags = windowFlags();

    flags |= Qt::WindowStaysOnTopHint;

    setWindowFlags(flags);

    // Setup the widgets.
    QString style = "QTreeWidget::item:!selected "
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
    messageTreeWidget->clear();

    // Get icons.
    _informationIcon = QIcon(":/seer/resources/RelaxLightIcons/data-information.svg");
    _warningIcon     = QIcon(":/seer/resources/RelaxLightIcons/data-warning.svg");
    _criticalIcon    = QIcon(":/seer/resources/RelaxLightIcons/data-error.svg");
    _questionIcon    = QIcon(":/seer/resources/RelaxLightIcons/dialog-question.svg");


    // Connect things.
    QObject::connect(okPushButton,      &QToolButton::clicked,              this,  &SeerMessagesWidget::handleOkButtonClicked);

    // Restore window settings.
    readSettings();

    // Hide right away.
    hide();
}

SeerMessagesWidget::~SeerMessagesWidget () {
}

void SeerMessagesWidget::addMessage (const QString& message, QMessageBox::Icon messageType) {

    show();
    raise();

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
}

void SeerMessagesWidget::handleOkButtonClicked () {

    hide();
}

void SeerMessagesWidget::writeSettings() {

    QSettings settings;

    settings.beginGroup("executionmessagesdialog"); {
        settings.setValue("size", size());
    }settings.endGroup();
}

void SeerMessagesWidget::readSettings() {

    QSettings settings;

    settings.beginGroup("executionmessagesdialog"); {
        resize(settings.value("size", QSize(425, 150)).toSize());
    } settings.endGroup();
}

void SeerMessagesWidget::resizeEvent (QResizeEvent* event) {

    // Write window settings.
    writeSettings();

    QWidget::resizeEvent(event);
}

