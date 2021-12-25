#include "SeerEditorConfigPage.h"
#include <QtGui/QFontDatabase>
#include <QtWidgets/QWidget>
#include <QtWidgets/QSpinBox>
#include <QtWidgets/QFontDialog>
#include <QtCore/QDebug>

SeerEditorConfigPage::SeerEditorConfigPage(QWidget* parent) : QWidget(parent) {

    // Set up the UI.
    setupUi(this);

    // Setup the widgets
    handleFontChanged(QFont("Source Code Pro"));

    // Connect things.
    QObject::connect(fontSizeComboBox,  &QComboBox::currentTextChanged,                 this, &SeerEditorConfigPage::handleSizeChanged);
    QObject::connect(fontNameComboBox,  &QFontComboBox::currentFontChanged,             this, &SeerEditorConfigPage::handleFontChanged);
    QObject::connect(fontDialogButton,  &QToolButton::clicked,                          this, &SeerEditorConfigPage::handleFontDialog);
}

SeerEditorConfigPage::~SeerEditorConfigPage() {
}

void SeerEditorConfigPage::handleSizeChanged (const QString& text) {

    // Convert the text size to a number.
    int size = text.toInt();

    // Guard against a bad size.
    if (size <= 0) {
        return;
    }

    // Set its size.
    _font.setPointSize(size);

    // Display our example text with the new font.
    codePlainTextEdit->setFont(_font);
}

void SeerEditorConfigPage::handleFontChanged (const QFont& font) {

    // Create a font database.
    QFontDatabase fontDatabase;

    // Clear the font size list.
    QStringList sizes;
    fontSizeComboBox->clear();

    // Populate the font size list from the given font.
    foreach (int points, fontDatabase.smoothSizes(font.family(), font.styleName())) {
        sizes.append(QString::number(points));
    }

    fontSizeComboBox->addItems(sizes);

    // Set the current font size in the list.
    fontSizeComboBox->setCurrentText(QString::number(font.pointSize()));

    // Set the current font family in the list.
    fontNameComboBox->setCurrentFont(font);

    // Display our example text with the new font.
    codePlainTextEdit->setFont(font);

    // Save the new font.
    _font = font;
}

void SeerEditorConfigPage::handleFontDialog () {

    bool ok;

    QFont font = QFontDialog::getFont(&ok, _font, this,  "Select a font for the editors", QFontDialog::MonospacedFonts);

    if (ok) {
        handleFontChanged(font);
    }
}

