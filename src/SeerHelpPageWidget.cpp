#include "SeerHelpPageWidget.h"
#include <QtPrintSupport/QPrinter>
#include <QtPrintSupport/QPrintDialog>
#include <QtCore/QFile>
#include <QtCore/QSettings>
#include <QtCore/QDebug>

SeerHelpPageWidget::SeerHelpPageWidget(QWidget* parent) : QWidget(parent) {

    // Construct the UI.
    setupUi(this);

    // Setup the widgets
    setWindowIcon(QIcon(":/seer/resources/seergdb_64x64.png"));
    setWindowTitle("Seer Help");
    setAttribute(Qt::WA_DeleteOnClose);

    textBrowser->setOpenExternalLinks(true);

    // Connect things.
    QObject::connect(printToolButton,  &QToolButton::clicked,          this,  &SeerHelpPageWidget::handlePrintToolButton);
    QObject::connect(okPushButton,     &QPushButton::clicked,          this,  &SeerHelpPageWidget::handleOkPushButton);

    // Restore window settings.
    readSettings();
}

SeerHelpPageWidget::~SeerHelpPageWidget() {
}

void SeerHelpPageWidget::loadFile (const QString& filename) {

    // Get the Help text from the file.
    QFile file(filename);
    file.open(QFile::ReadOnly|QFile::Text);

    QTextStream stream(&file);

    QString text = stream.readAll();

    // Load it into the browser.
    loadText(text);
}

void SeerHelpPageWidget::loadText (const QString& text) {

    // Put the Help text in as markdown. Move back to the begining.
#if (QT_VERSION >= QT_VERSION_CHECK(5, 14, 0))
    textBrowser->setMarkdown(text);
#else
    textBrowser->setText(text);
#endif
    textBrowser->moveCursor(QTextCursor::Start);
}

void SeerHelpPageWidget::handlePrintToolButton () {

    QPrinter printer;

    QPrintDialog* dlg = new QPrintDialog(&printer, this);

    if (dlg->exec() != QDialog::Accepted) {
        return;
    }

    QTextDocument* document = textBrowser->document();

    document->print(&printer);
}

void SeerHelpPageWidget::handleOkPushButton () {

    close();
}

void SeerHelpPageWidget::writeSettings() {

    QSettings settings;

    settings.beginGroup("helpwindow"); {
        settings.setValue("size", size());
    }settings.endGroup();
}

void SeerHelpPageWidget::readSettings() {

    QSettings settings;

    settings.beginGroup("helpwindow"); {
        resize(settings.value("size", QSize(600, 600)).toSize());
    }settings.endGroup();
}

void SeerHelpPageWidget::resizeEvent (QResizeEvent* event) {

    // Write window settings.
    writeSettings();

    QWidget::resizeEvent(event);
}

