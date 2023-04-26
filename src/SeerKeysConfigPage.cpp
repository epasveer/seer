#include "SeerKeysConfigPage.h"
#include <QtWidgets/QKeySequenceEdit>
#include <QtWidgets/QLabel>
#include <QtWidgets/QWidget>
#include <QtCore/QDebug>

SeerKeysConfigPage::SeerKeysConfigPage(QWidget* parent) : QWidget(parent) {

    // Set up the UI.
    setupUi(this);

    // Connect things.

    // Setup the widgets
    setKeySettings(SeerKeySettings::populate());
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

    if (keysTableWidget->rowCount() == 0) {
        return settings;
    }

    if (keysTableWidget->columnCount() == 2) {
        return settings;
    }

    for (int r=0; r<keysTableWidget->rowCount(); r++) {

        // Get the key (label) for this row.
        QString key = keysTableWidget->verticalHeaderItem(r)->text();

        // Get widgets for this row.
        QKeySequenceEdit* keySequenceEdit  = dynamic_cast<QKeySequenceEdit*>(keysTableWidget->cellWidget(r,0));
        QLabel*           descriptionLabel = dynamic_cast<QLabel*>(keysTableWidget->cellWidget(r,1));

        // Create key setting.
        if (keySequenceEdit != 0 && descriptionLabel != 0) {

            SeerKeySetting setting(key, keySequenceEdit->keySequence(), descriptionLabel->text());

            // Add the setting to our settings.
            settings.add(key, setting);

        }else{

            if (keySequenceEdit == 0) {
                qDebug() << "QKeySequenceEdit for row" << r << "is null!";
            }

            if (descriptionLabel == 0) {
                qDebug() << "QLabel for row" << r << "is null!";
            }
        }
    }

    return settings;
}

void SeerKeysConfigPage::reset () {

    setKeySettings(SeerKeySettings::populate());
}

