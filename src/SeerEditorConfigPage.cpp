#include "SeerEditorConfigPage.h"
#include "QColorButton.h"
#include <QtGui/QFontDatabase>
#include <QtWidgets/QWidget>
#include <QtWidgets/QSpinBox>
#include <QtWidgets/QFontDialog>
#include <QtWidgets/QComboBox>
#include <QtCore/QDebug>

SeerEditorConfigPage::SeerEditorConfigPage(QWidget* parent) : QWidget(parent) {

    // Set up the UI.
    setupUi(this);

    //
    // Setup the widgets
    //

    // Set example text.
    editorWidget->sourceArea()->openText("/*\n"
                                         " * Seer, Copyright 2021 (c)\n"
                                         " * Ernie Pasveer (epasveer@att.net)\n"
                                         " */\n"
                                         "int main(int argc, char* argv[]) {\n"
                                         "\n"
                                         "    std::cout << \"Hello, Seer!\"; // Greetings\n"
                                         "\n"
                                         "    return 0;\n"
                                         "}",
                                         "sample.cpp");

    // Connect things.
    QObject::connect(fontSizeComboBox,            &QComboBox::currentTextChanged,           this, &SeerEditorConfigPage::handleFontSizeChanged);
    QObject::connect(fontNameComboBox,            &QFontComboBox::currentFontChanged,       this, &SeerEditorConfigPage::handleFontChanged);
    QObject::connect(fontDialogButton,            &QToolButton::clicked,                    this, &SeerEditorConfigPage::handleFontDialog);
    QObject::connect(highlighterEnabledCheckBox,  &QToolButton::clicked,                    this, &SeerEditorConfigPage::handleEnabledChanged);
    QObject::connect(highlighterSuffixesLineEdit, &QHistoryLineEdit::lostFocus,             this, &SeerEditorConfigPage::handleHighlighterChanged);
    QObject::connect(themeApplyToolButton,        &QToolButton::clicked,                    this, &SeerEditorConfigPage::handleApplyTheme);

    // Set the defaults.
    reset();

    // Fill in the available theme names.
    themeComboBox->addItems(SeerHighlighterSettings::themeNames());
}

SeerEditorConfigPage::~SeerEditorConfigPage() {
}

void SeerEditorConfigPage::setEditorFont (const QFont& font) {

    handleFontChanged(font);
}

const QFont& SeerEditorConfigPage::editorFont () const {

    return _font;
}

void SeerEditorConfigPage::setEditorTabSize (int spaces) {

    tabSpinBox->setValue(spaces);
}

int SeerEditorConfigPage::editorTabSize () const {

    return tabSpinBox->value();
}

void SeerEditorConfigPage::setHighlighterSettings (const SeerHighlighterSettings& settings) {

    _highlighterSettings = settings;

    // Clear the table contents.
    highlighterTableWidget->setRowCount(0);

    // Get a list of keys from the highlighter.
    QStringList keys = _highlighterSettings.keys();

    // Loop through each key and get its info.
    // Construct a table entry.
    for (int r=0; r<keys.size(); r++) {

        QString key = keys[r];

        QTextCharFormat format = _highlighterSettings.get(key);

        highlighterTableWidget->insertRow(r);

        // Insert the font weight.
        QComboBox* fontWeightBox = new QComboBox;
        fontWeightBox->addItem("Normal");
        fontWeightBox->addItem("Bold");

        if (format.fontWeight() == QFont::Normal) {
            fontWeightBox->setCurrentText("Normal");
        }else if (format.fontWeight() == QFont::Bold) {
            fontWeightBox->setCurrentText("Bold");
        }else{
            fontWeightBox->setCurrentText("Normal");
        }

        highlighterTableWidget->setCellWidget(r, 0, fontWeightBox);

        // Insert the font italic.
        QComboBox* fontItalicBox = new QComboBox;
        fontItalicBox->addItem("Normal");
        fontItalicBox->addItem("Italic");

        if (format.fontItalic() == false) {
            fontItalicBox->setCurrentText("Normal");
        }else if (format.fontItalic() == true) {
            fontItalicBox->setCurrentText("Italic");
        }else{
            fontItalicBox->setCurrentText("Normal");
        }

        highlighterTableWidget->setCellWidget(r, 1, fontItalicBox);

        // Insert the font color.
        QColorButton* foregroundColorButton = new QColorButton;
        foregroundColorButton->setColor(format.foreground().color());

        QColorButton* backgroundColorButton = new QColorButton;
        backgroundColorButton->setColor(format.background().color());

        highlighterTableWidget->setCellWidget(r, 2, foregroundColorButton);
        highlighterTableWidget->setCellWidget(r, 3, backgroundColorButton);

        // Connect things to watch for changes.
        QObject::connect(fontWeightBox,                 QOverload<int>::of(&QComboBox::currentIndexChanged),         this, &SeerEditorConfigPage::handleHighlighterChanged);
        QObject::connect(fontItalicBox,                 QOverload<int>::of(&QComboBox::currentIndexChanged),         this, &SeerEditorConfigPage::handleHighlighterChanged);
        QObject::connect(foregroundColorButton,         &QColorButton::colorChanged,                                 this, &SeerEditorConfigPage::handleHighlighterChanged);
        QObject::connect(backgroundColorButton,         &QColorButton::colorChanged,                                 this, &SeerEditorConfigPage::handleHighlighterChanged);
    }

    highlighterTableWidget->setVerticalHeaderLabels(keys);

    highlighterTableWidget->resizeColumnToContents(0); // Weight
    highlighterTableWidget->resizeColumnToContents(1); // Italic
    highlighterTableWidget->resizeColumnToContents(2); // Foreground color
    highlighterTableWidget->resizeColumnToContents(3); // Background color

    highlighterSuffixesLineEdit->setText(_highlighterSettings.sourceSuffixes());
    highlighterSuffixesLineEdit->setCursorPosition(0);

    // Update our sample editor.
    editorWidget->sourceArea()->setHighlighterSettings(highlighterSettings());
    editorWidget->sourceArea()->setCurrentLine(9);
}

const SeerHighlighterSettings& SeerEditorConfigPage::highlighterSettings() const {

    return _highlighterSettings;
}

void SeerEditorConfigPage::setHighlighterEnabled (bool flag) {

    highlighterEnabledCheckBox->setChecked(flag);

    editorWidget->sourceArea()->setHighlighterEnabled(flag);
}

bool SeerEditorConfigPage::highlighterEnabled () const {

    return editorWidget->sourceArea()->highlighterEnabled();
}

void SeerEditorConfigPage::setExternalEditorCommand (const QString& externalEditorCommand) {

    externalEditorCommandLineEdit->setText(externalEditorCommand);
}

QString SeerEditorConfigPage::externalEditorCommand () const {

    return externalEditorCommandLineEdit->text();
}

void SeerEditorConfigPage::reset () {

    setEditorFont(QFont("monospace", 10));
    setEditorTabSize(4);
    setHighlighterSettings(SeerHighlighterSettings::populateForCPP(""));
    setHighlighterEnabled(true);
    setExternalEditorCommand("");
}

void SeerEditorConfigPage::handleFontSizeChanged (const QString& text) {

    // Convert the text size to a number.
    int size = text.toInt();

    // Guard against a bad size.
    if (size <= 0) {
        return;
    }

    // Set its size.
    _font.setPointSize(size);

    // Display our example text with the new font.
    editorWidget->sourceArea()->setFont(_font);
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
    editorWidget->sourceArea()->setFont(font);

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

void SeerEditorConfigPage::handleHighlighterChanged () {

    SeerHighlighterSettings cppSettings;

    for (int r=0; r<highlighterTableWidget->rowCount(); r++) {

        // Get the key (label) for this row.
        QString key = highlighterTableWidget->verticalHeaderItem(r)->text();

        // Get widgets for this row.
        QComboBox*    fontWeightBox         = dynamic_cast<QComboBox*>(highlighterTableWidget->cellWidget(r,0));
        QComboBox*    fontItalicBox         = dynamic_cast<QComboBox*>(highlighterTableWidget->cellWidget(r,1));
        QColorButton* foregroundColorButton = dynamic_cast<QColorButton*>(highlighterTableWidget->cellWidget(r,2));
        QColorButton* backgroundColorButton = dynamic_cast<QColorButton*>(highlighterTableWidget->cellWidget(r,3));

        // Create an empty format.
        QTextCharFormat format;

        // Set the font weight.
        if (fontWeightBox->currentText() == "Normal") {
            format.setFontWeight(QFont::Normal);
        }else if (fontWeightBox->currentText() == "Bold") {
            format.setFontWeight(QFont::Bold);
        }else{
            format.setFontWeight(QFont::Normal);
        }

        // Set the font italic.
        if (fontItalicBox->currentText() == "Normal") {
            format.setFontItalic(false);
        }else if (fontItalicBox->currentText() == "Italic") {
            format.setFontItalic(true);
        }else{
            format.setFontItalic(false);
        }

        // Set the font color.
        format.setForeground(foregroundColorButton->color());
        format.setBackground(backgroundColorButton->color());

        // Add the format to our settings.
        cppSettings.add(key, format);
    }

    // Get list of source suffixes.
    cppSettings.setSourceSuffixes(highlighterSuffixesLineEdit->text());

    // Update our view.
    setHighlighterSettings(cppSettings);
}

void SeerEditorConfigPage::handleEnabledChanged () {

    setHighlighterEnabled(highlighterEnabledCheckBox->isChecked());
}

void SeerEditorConfigPage::handleApplyTheme () {

    setHighlighterSettings(SeerHighlighterSettings::populateForCPP(themeComboBox->currentText()));
}

