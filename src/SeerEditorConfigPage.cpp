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
    editorWidget->sourceArea()->openText("//\n"
                                         "// Seer, Copyright 2021 (c)\n"
                                         "// Ernie Pasveer (epasveer@att.net)\n"
                                         "//\n"
                                         "int main(int argc, char* argv[]) {\n"
                                         "\n"
                                         "    std::cout << \"Hello, Seer!\"; // Greetings\n"
                                         "\n"
                                         "    return 0;\n"
                                         "}",
                                         "sample.cpp");
    // Setup highlighter table.
    /*
    for (int r=0; r<highlighterTableWidget->rowCount(); r++) {

        QComboBox* fontWeightBox = new QComboBox;
        fontWeightBox->addItem("Normal");
        fontWeightBox->addItem("Bold");

        highlighterTableWidget->setCellWidget(r, 0, fontWeightBox);

        QComboBox* fontStyleBox = new QComboBox;
        fontStyleBox->addItem("Normal");
        fontStyleBox->addItem("Italic");

        highlighterTableWidget->setCellWidget(r, 1, fontStyleBox);

        QColorButton* fontColorButton = new QColorButton;

        highlighterTableWidget->setCellWidget(r, 2, fontColorButton);

        // If it's the first row, don't eanable weight or style.
        // It doesn't make sense for the background.
        if (r == 0) {
            fontWeightBox->setEnabled(false);
            fontStyleBox->setEnabled(false);
        }
    }
    */

    // Connect things.
    QObject::connect(fontSizeComboBox,  &QComboBox::currentTextChanged,                 this, &SeerEditorConfigPage::handleFontSizeChanged);
    QObject::connect(fontNameComboBox,  &QFontComboBox::currentFontChanged,             this, &SeerEditorConfigPage::handleFontChanged);
    QObject::connect(fontDialogButton,  &QToolButton::clicked,                          this, &SeerEditorConfigPage::handleFontDialog);

    // Set the default font and highlighter.
    handleFontChanged(QFont("Source Code Pro", 10));
    setHighlighterSettings(SeerHighlighterSettings::populateForCPP());
}

SeerEditorConfigPage::~SeerEditorConfigPage() {
}

void SeerEditorConfigPage::setEditorFont (const QFont& font) {

    handleFontChanged(font);
}

const QFont& SeerEditorConfigPage::editorFont () const {

    return _font;
}

void SeerEditorConfigPage::setHighlighterSettings (const SeerHighlighterSettings& settings) {

    _highlighterSettings = settings;

    // Clear the table contents.
    highlighterTableWidget->clearContents();

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
        QColorButton* fontColorButton = new QColorButton;
        fontColorButton->setColor(format.foreground().color());

        highlighterTableWidget->setCellWidget(r, 2, fontColorButton);
    }

    highlighterTableWidget->setVerticalHeaderLabels(keys);

    highlighterTableWidget->resizeColumnToContents(0); // Weight
    highlighterTableWidget->resizeColumnToContents(1); // Italic
    highlighterTableWidget->resizeColumnToContents(2); // Color
}

const SeerHighlighterSettings& SeerEditorConfigPage::highlighterSettings() const {

    return _highlighterSettings;
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

// class
// quotation
// function
// comment
// multiline comment
// keyword

//  5     // Define formats.
//  6     _classFormat.setFontWeight(QFont::Bold);
//  7     _classFormat.setForeground(Qt::darkMagenta);
//  8     _quotationFormat.setForeground(Qt::darkGreen);
//  9     _functionFormat.setFontItalic(true);
// 10     _functionFormat.setForeground(Qt::blue);
// 11     _singleLineCommentFormat.setForeground(Qt::red);
// 12     _multiLineCommentFormat.setForeground(Qt::red);
// 13     _keywordFormat.setForeground(Qt::darkBlue);
// 14     _keywordFormat.setFontWeight(QFont::Bold);

