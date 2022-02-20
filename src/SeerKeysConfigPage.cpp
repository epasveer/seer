#include "SeerKeysConfigPage.h"
#include <QtWidgets/QKeySequenceEdit>
#include <QtWidgets/QLabel>
#include <QtWidgets/QWidget>

SeerKeysConfigPage::SeerKeysConfigPage(QWidget* parent) : QWidget(parent) {

    // Set up the UI.
    setupUi(this);

    // Setup the widgets
    setKeySettings(SeerKeySettings::populate());

    // Connect things.
}

SeerKeysConfigPage::~SeerKeysConfigPage() {
}

void SeerKeysConfigPage::setKeySettings (const SeerKeySettings& settings) {

    // Clear the table contents.
    keysTableWidget->setRowCount(0);

    // Get a list of keys from the highlighter.
    QStringList keys = settings.keys();

    // Loop through each key and get its info.
    // Construct a table entry.
    for (int r=0; r<keys.size(); r++) {

        QString key = keys[r];

        SeerKeySetting setting = settings.get(key);

        keysTableWidget->insertRow(r);

        // Insert the KeySequence editor.
        QKeySequenceEdit* keySequenceEdit = new QKeySequenceEdit;
        keySequenceEdit->setKeySequence(setting._sequence);

        keysTableWidget->setCellWidget(r, 0, keySequenceEdit);

        // Insert the Description.
        QLabel* descriptionLabel = new QLabel(setting._description);
        keysTableWidget->setCellWidget(r, 1, descriptionLabel);
    }

    keysTableWidget->setVerticalHeaderLabels(keys);

    keysTableWidget->resizeColumnToContents(0); // KeySequence
    keysTableWidget->resizeColumnToContents(1); // Description
}

SeerKeySettings SeerKeysConfigPage::keySettings() const {

    SeerKeySettings settings;

    for (int r=0; r<keysTableWidget->rowCount(); r++) {

        // Get the key (label) for this row.
        QString key = keysTableWidget->verticalHeaderItem(r)->text();

        // Get widgets for this row.
        QKeySequenceEdit* keySequenceEdit  = dynamic_cast<QKeySequenceEdit*>(keysTableWidget->cellWidget(r,0));
        QLabel*           descriptionLabel = dynamic_cast<QLabel*>(keysTableWidget->cellWidget(r,1));

        // Create key setting.
        SeerKeySetting setting(key, keySequenceEdit->keySequence(), descriptionLabel->text());

        // Add the setting to our settings.
        settings.add(key, setting);
    }

    return settings;
}

