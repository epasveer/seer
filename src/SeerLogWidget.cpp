#include "SeerLogWidget.h"
#include <QtWidgets/QTextEdit>
#include <QtWidgets/QCheckBox>
#include <QtWidgets/QFileDialog>
#include <QtWidgets/QMessageBox>
#include <QtWidgets/QScrollBar>
#include <QtPrintSupport/QPrinter>
#include <QtPrintSupport/QPrintDialog>
#include <QtGui/QFont>
#include <QtCore/QTextStream>

SeerLogWidget::SeerLogWidget (QWidget* parent) : QWidget(parent) {

    setupUi(this);

    // Setup the widgets
    QFont font;
    font.setFamily("monospace [Consolas]");
    font.setFixedPitch(true);
    font.setStyleHint(QFont::TypeWriter);

    textEdit->setReadOnly(true);
    textEdit->setFont(font);
    textEdit->setLineWrapMode(QTextEdit::NoWrap);   // No wrap
    wrapTextCheckBox->setCheckState(Qt::Unchecked); // No wrap

    // Connect things.
    QObject::connect(clearButton,       &QPushButton::clicked,      this,  &SeerLogWidget::handleClearButton);
    QObject::connect(printButton,       &QPushButton::clicked,      this,  &SeerLogWidget::handlePrintButton);
    QObject::connect(saveButton,        &QPushButton::clicked,      this,  &SeerLogWidget::handleSaveButton);
    QObject::connect(wrapTextCheckBox,  &QCheckBox::clicked,        this,  &SeerLogWidget::handleWrapTextCheckBox);
}

SeerLogWidget::~SeerLogWidget () {
}

void SeerLogWidget::processText (const QString& text) {

    // Add text to the end of the document.
    textEdit->append(text);

    moveToEnd();
}

void SeerLogWidget::moveToEnd () {

    // Move cursor to start of the last line.
    // Move scrollbars to the last line.
    int nlines = textEdit->document()->lineCount();

    QTextCursor cursor = textEdit->textCursor();

    cursor.setPosition(nlines-1);

    textEdit->setTextCursor(cursor);
    textEdit->moveCursor(QTextCursor::StartOfLine);

    textEdit->verticalScrollBar()->setValue(textEdit->verticalScrollBar()->maximum());
}

void SeerLogWidget::handleText (const QString& text) {

    processText(text);
}

void SeerLogWidget::handleClearButton () {
    textEdit->clear();
}

void SeerLogWidget::handlePrintButton () {

    QPrinter printer;

    QPrintDialog* dlg = new QPrintDialog(&printer, this);

    if (dlg->exec() != QDialog::Accepted) {
        return;
    }

    QTextDocument* document = textEdit->document();

    document->print(&printer);
}

void SeerLogWidget::handleSaveButton () {

    /*
    QString fname = QFileDialog::getSaveFileName(this, "Seer log file", "", "Logs (*.log);;Text files (*.txt);;All files (*.*)");
    */

    QFileDialog dialog(this, "Seer log file", "", "Logs (*.log);;Text files (*.txt);;All files (*.*)");
    dialog.setOptions(QFileDialog::DontUseNativeDialog);
    dialog.setAcceptMode(QFileDialog::AcceptSave);
    dialog.setFileMode(QFileDialog::AnyFile);
    dialog.setDefaultSuffix("log");
    dialog.selectFile("gdboutput.log");

    if (dialog.exec() != QDialog::Accepted) {
        return;
    }

    QStringList files = dialog.selectedFiles();

    if (files.size() == 0) {
        return;
    }

    if (files.size() > 1) {
        QMessageBox::critical(this, tr("Error"), tr("Select only 1 file."));
        return;
    }

    QFile file(files[0]);

    if (file.open(QIODevice::ReadWrite)) {
        QTextStream stream(&file);
        stream << textEdit->toPlainText();
        file.flush();
        file.close();

    }else{
        QMessageBox::critical(this, tr("Error"), tr("Cannot save log to file."));
        return;
    }
}

void SeerLogWidget::handleWrapTextCheckBox () {

    if (wrapTextCheckBox->checkState() == Qt::Unchecked) {
        textEdit->setLineWrapMode(QTextEdit::NoWrap);       // No wrap
    }else{
        textEdit->setLineWrapMode(QTextEdit::WidgetWidth);  // Wrap at end of widget
    }
}

