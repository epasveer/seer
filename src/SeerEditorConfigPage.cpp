#include "SeerEditorConfigPage.h"
#include <QtWidgets/QWidget>
#include <QtWidgets/QSpinBox>
#include <QtWidgets/QFontDialog>

SeerEditorConfigPage::SeerEditorConfigPage(QWidget* parent) : QWidget(parent) {

    // Set up the UI.
    setupUi(this);

    // Setup the widgets
    _font = QFont("Source Code Pro");

    fontComboBox->setCurrentFont(_font);
    fontSpinBox->setValue(_font.pointSize());
    codePlainTextEdit->setFont(_font);

    // Connect things.
    QObject::connect(fontSpinBox,  QOverload<int>::of(&QSpinBox::valueChanged),      this, &SeerEditorConfigPage::handlePointSizeChanged);
    QObject::connect(fontComboBox, &QFontComboBox::currentFontChanged,               this, &SeerEditorConfigPage::handleFontChanged);
    QObject::connect(fontButton,   &QToolButton::clicked,                            this, &SeerEditorConfigPage::handleFontDialog);
}

SeerEditorConfigPage::~SeerEditorConfigPage() {
}

void SeerEditorConfigPage::handlePointSizeChanged (int i) {

    _font.setPointSize(i);

    _updateCodeTextEdit();
}

void SeerEditorConfigPage::handleFontChanged (const QFont& font) {

    // Save the original point size before changing the font.
    int oldPointSize = _font.pointSize();

    _font = font;

    _font.setPointSize(oldPointSize);

    _updateCodeTextEdit();
}

void SeerEditorConfigPage::handleFontDialog () {

    bool ok;

    QFont font = QFontDialog::getFont(&ok, _font, this,  "Select a font for the editor", QFontDialog::MonospacedFonts);

    if (ok) {
        handleFontChanged(font);
    }
}

void SeerEditorConfigPage::_updateCodeTextEdit () {

    fontComboBox->setCurrentFont(_font);
    codePlainTextEdit->setFont(_font);
    fontSpinBox->setValue(_font.pointSize());
}

