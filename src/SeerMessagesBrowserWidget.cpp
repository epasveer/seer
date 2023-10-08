#include "SeerMessagesBrowserWidget.h"
#include <QtWidgets/QTreeWidget>
#include <QtWidgets/QTreeWidgetItemIterator>
#include <QtCore/QSettings>
#include <QtCore/QTime>
#include <QtCore/QDebug>

SeerMessagesBrowserWidget::SeerMessagesBrowserWidget (QWidget* parent) : QWidget(parent) {

    _raiseMode = "any";  // Default. Raise message tab on any message.
    _raiseMenu = 0;

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


    _raiseMenu = new QMenu("Raise Messages Tab");

    QMenu* menu = new QMenu();
    menu->addMenu(_raiseMenu);

    _anyMessageAction       = _raiseMenu->addAction("Any message");
    _importanMessagesAction = _raiseMenu->addAction("Important messages");
    _neverMessagesAction    = _raiseMenu->addAction("Never");

    preferencesToolButton->setMenu(menu);
    preferencesToolButton->setPopupMode(QToolButton::InstantPopup);

    // Get icons.
    _informationIcon = QIcon(":/seer/resources/RelaxLightIcons/data-information.svg");
    _warningIcon     = QIcon(":/seer/resources/RelaxLightIcons/data-warning.svg");
    _criticalIcon    = QIcon(":/seer/resources/RelaxLightIcons/data-error.svg");
    _questionIcon    = QIcon(":/seer/resources/RelaxLightIcons/dialog-question.svg");


    // Connect things.
    QObject::connect(deleteMessagesToolButton,   &QToolButton::clicked,         this,  &SeerMessagesBrowserWidget::handleDeleteToolButton);
    QObject::connect(_raiseMenu,                 &QMenu::aboutToShow,           this,  &SeerMessagesBrowserWidget::handleRaiseMenuShow);
    QObject::connect(_raiseMenu,                 &QMenu::triggered,             this,  &SeerMessagesBrowserWidget::handleRaiseMenuTriggered);

    // Clear messages
    clearMessages();

    // Read the default settings.
    readSettings();
}

SeerMessagesBrowserWidget::~SeerMessagesBrowserWidget () {
}

void SeerMessagesBrowserWidget::addMessage (const QString& message, QMessageBox::Icon messageType) {

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

    // Signal that a message was added, depending on the 'raise' mode.
    if (_raiseMode == "any") {
        emit showMessages();
    }else if (_raiseMode == "important") {
        if (messageType == QMessageBox::Warning || messageType == QMessageBox::Critical || messageType == QMessageBox::Question) {
            emit showMessages();
        }else if (message.startsWith("Program started")) {
            emit showMessages();
        }else if (message.startsWith("Program exited")) {
            emit showMessages();
        }
    }else if (_raiseMode == "never") {
        // Do nothing.
    }
}

void SeerMessagesBrowserWidget::clearMessages () {

    messagesTreeWidget->clear();
}

void SeerMessagesBrowserWidget::writeSettings () {

    QSettings settings;

    settings.beginGroup("executionmessages"); {
        settings.setValue("raisetabmode", _raiseMode);
    }settings.endGroup();
}

void SeerMessagesBrowserWidget::readSettings () {

    QSettings settings;

    settings.beginGroup("executionmessages"); {
        _raiseMode = settings.value("raisetabmode", "any").toString();
    } settings.endGroup();
}

void SeerMessagesBrowserWidget::handleDeleteToolButton () {

    // Delete all messages.
    clearMessages();
}

void SeerMessagesBrowserWidget::handleRaiseMenuShow () {

    if (_raiseMode == "any") {
        _raiseMenu->setDefaultAction(_anyMessageAction);
    }else if (_raiseMode == "important") {
        _raiseMenu->setDefaultAction(_importanMessagesAction);
    }else if (_raiseMode == "never") {
        _raiseMenu->setDefaultAction(_neverMessagesAction);
    }else{
        _raiseMenu->setDefaultAction(_anyMessageAction);
    }
}

void SeerMessagesBrowserWidget::handleRaiseMenuTriggered (QAction* action) {

    if (action == _anyMessageAction) {
        _raiseMode = "any";
    }else if (action == _importanMessagesAction) {
        _raiseMode = "important";
    }else if (action == _neverMessagesAction) {
        _raiseMode = "never";
    }else{
        _raiseMode = "any";
    }

    writeSettings();
}

