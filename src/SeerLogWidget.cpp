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
#include <QtCore/QDebug>

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
    QObject::connect(enableCheckBox,    &QCheckBox::clicked,        this,  &SeerLogWidget::handleEnableCheckBox);
}

SeerLogWidget::~SeerLogWidget () {
}

void SeerLogWidget::processText (const QString& text) {

    // Add text to the end of the document.
    textEdit->append(text);
}

bool SeerLogWidget::isLogEnabled () const {

    return enableCheckBox->isChecked();
}

void SeerLogWidget::setLogEnabled (bool flag) {

    enableCheckBox->setChecked(flag);
}

void SeerLogWidget::moveToEnd () {

    // Move to the end and then to the beginning of that line.
    QTextCursor cursor = textEdit->textCursor();
    textEdit->moveCursor(QTextCursor::End,          QTextCursor::MoveAnchor);
    textEdit->moveCursor(QTextCursor::StartOfBlock, QTextCursor::MoveAnchor);

    textEdit->verticalScrollBar()->setValue(textEdit->verticalScrollBar()->maximum());
}

void SeerLogWidget::setPlaceholderText (const QString& text) {

    textEdit->setPlaceholderText(text);
}

void SeerLogWidget::handleText (const QString& text) {

    // Don't do anything if we're not enabled.
    if (isLogEnabled() == false) {
        return;
    }

    // Process the text.
    processText(text);

    // Move to the end of the document.
    moveToEnd();
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

    QFileDialog dialog(this, "Seer log file", "./", "Logs (*.log);;Text files (*.txt);;All files (*.*)");
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

void SeerLogWidget::handleEnableCheckBox () {

    //qDebug() << "Enabled clicked!";

    emit logEnabledChanged(enableCheckBox->isChecked());
}

