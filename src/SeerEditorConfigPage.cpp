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
    editorWidget->sourceArea()->openText("int main(int argc, char* argv[]) {\n"
                                         "\n"
                                         "    std::cout << \"Hello, Seer!\";\n"
                                         "\n"
                                         "    return 0;\n"
                                         "}",
                                         "sample.cpp");
    // Setup syntax table.
    for (int r=0; r<syntaxTableWidget->rowCount(); r++) {

        QComboBox* fontWeightBox = new QComboBox;
        fontWeightBox->addItem("Normal");
        fontWeightBox->addItem("Bold");

        syntaxTableWidget->setCellWidget(r, 0, fontWeightBox);

        QComboBox* fontStyleBox = new QComboBox;
        fontStyleBox->addItem("Normal");
        fontStyleBox->addItem("Italic");

        syntaxTableWidget->setCellWidget(r, 1, fontStyleBox);

        QColorButton* fontColorButton = new QColorButton;

        syntaxTableWidget->setCellWidget(r, 2, fontColorButton);
    }

    // Connect things.
    QObject::connect(fontSizeComboBox,  &QComboBox::currentTextChanged,                 this, &SeerEditorConfigPage::handleSizeChanged);
    QObject::connect(fontNameComboBox,  &QFontComboBox::currentFontChanged,             this, &SeerEditorConfigPage::handleFontChanged);
    QObject::connect(fontDialogButton,  &QToolButton::clicked,                          this, &SeerEditorConfigPage::handleFontDialog);

    // Set the default font.
    handleFontChanged(QFont("Source Code Pro", 10));
}

SeerEditorConfigPage::~SeerEditorConfigPage() {
}

void SeerEditorConfigPage::setEditorFont (const QFont& font) {

    handleFontChanged(font);
}

const QFont& SeerEditorConfigPage::editorFont () const {

    return _font;
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

