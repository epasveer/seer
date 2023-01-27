#include "SeerHelpPageDialog.h"
#include <QtPrintSupport/QPrinter>
#include <QtPrintSupport/QPrintDialog>
#include <QtGui/QIcon>
#include <QtCore/QFile>
#include <QtCore/QSettings>
#include <QtCore/QDebug>

SeerHelpPageDialog::SeerHelpPageDialog(QDialog* parent) : QDialog(parent) {

    // Construct the UI.
    setupUi(this);

    // Setup the widgets
    setWindowIcon(QIcon(":/seer/resources/seergdb_64x64.png"));
    setWindowTitle("Seer Help");
    setAttribute(Qt::WA_DeleteOnClose);

    textBrowser->setOpenExternalLinks(true);

    // Connect things.

    // Restore window settings.
    readSettings();
}

SeerHelpPageDialog::~SeerHelpPageDialog() {
}

void SeerHelpPageDialog::loadFile (const QString& filename) {

    // Get the Help text from the file.
    QFile file(filename);
    file.open(QFile::ReadOnly|QFile::Text);

    QTextStream stream(&file);

    QString text = stream.readAll();

    // Load it into the browser.
    loadText(text);
}

void SeerHelpPageDialog::loadText (const QString& text) {

    // Put the Help text in as markdown. Move back to the begining.
#if (QT_VERSION >= QT_VERSION_CHECK(5, 14, 0))
    textBrowser->setMarkdown(text);
#else
    textBrowser->setText(text);
#endif
    textBrowser->moveCursor(QTextCursor::Start);
}

void SeerHelpPageDialog::writeSettings() {

    QSettings settings;

    settings.beginGroup("helpwindow"); {
        settings.setValue("size", size());
    }settings.endGroup();
}

void SeerHelpPageDialog::readSettings() {

    QSettings settings;

    settings.beginGroup("helpwindow"); {
        resize(settings.value("size", QSize(600, 600)).toSize());
    }settings.endGroup();
}

void SeerHelpPageDialog::resizeEvent (QResizeEvent* event) {

    // Write window settings.
    writeSettings();

    QDialog::resizeEvent(event);
}

