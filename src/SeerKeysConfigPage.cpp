#include "SeerKeysConfigPage.h"
#include <QtWidgets/QKeySequenceEdit>
#include <QtWidgets/QLabel>
#include <QtWidgets/QWidget>

SeerKeysConfigPage::SeerKeysConfigPage(QWidget* parent) : QWidget(parent) {

    // Set up the UI.
    setupUi(this);

    // Setup the widgets

    // Connect things.
}

SeerKeysConfigPage::~SeerKeysConfigPage() {
}

void SeerKeysConfigPage::setKeySettings (const SeerKeySettings& settings) {

    _keySettings = settings;

    // Clear the table contents.
    keysTableWidget->setRowCount(0);

    // Get a list of keys from the highlighter.
    QStringList keys = _keySettings.keys();

    // Loop through each key and get its info.
    // Construct a table entry.
    for (int r=0; r<keys.size(); r++) {

        QString key = keys[r];

        SeerKeySetting setting = _keySettings.get(key);

        keysTableWidget->insertRow(r);

        // Insert the KeySequence editor.
        QKeySequenceEdit* keySequenceEdit = new QKeySequenceEdit;
        keySequenceEdit->setKeySequence(setting._sequence);

        keysTableWidget->setCellWidget(r, 0, keySequenceEdit);

        // Insert the Description.
        QLabel* descriptionLabel = new QLabel(setting._help);

        keysTableWidget->setCellWidget(r, 1, descriptionLabel);

        // Connect things to watch for changes.
        //QObject::connect(fontWeightBox,             QOverload<int>::of(&QComboBox::currentIndexChanged),         this, &SeerEditorConfigPage::handleHighlighterChanged);
        //QObject::connect(fontItalicBox,             QOverload<int>::of(&QComboBox::currentIndexChanged),         this, &SeerEditorConfigPage::handleHighlighterChanged);
        //QObject::connect(foregroundColorButton,     &QColorButton::colorChanged,                                 this, &SeerEditorConfigPage::handleHighlighterChanged);
        //QObject::connect(backgroundColorButton,     &QColorButton::colorChanged,                                 this, &SeerEditorConfigPage::handleHighlighterChanged);
    }

    keysTableWidget->setVerticalHeaderLabels(keys);

    keysTableWidget->resizeColumnToContents(0); // KeySequence
    keysTableWidget->resizeColumnToContents(1); // Description
}

const SeerKeySettings& SeerKeysConfigPage::keySettings() const {

    return _keySettings;
}

