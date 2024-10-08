#include "SeerConsoleWidget.h"
#include <QtWidgets/QPlainTextEdit>
#include <QtWidgets/QCheckBox>
#include <QtWidgets/QFileDialog>
#include <QtWidgets/QFontDialog>
#include <QtWidgets/QMessageBox>
#include <QtWidgets/QScrollBar>
#include <QtPrintSupport/QPrinter>
#include <QtPrintSupport/QPrintDialog>
#include <QtGui/QFont>
#include <QtGui/QIcon>
#include <QtCore/QTextStream>
#include <QtCore/QSettings>
#include <QtCore/QDebug>
#include <unistd.h>
#include <stdlib.h>
#include <fcntl.h>
#include <termios.h>
#include <sys/ioctl.h>
#include <unistd.h>

SeerConsoleWidget::SeerConsoleWidget (QWidget* parent) : QWidget(parent) {

    // Init variables.
    _ttyDeviceName = "";
    _ptsFD         = -1;
    _ptsListener   = 0;
    _enableStdout  = false;
    _enableWrap    = false;

    // Set up UI.
    setupUi(this);

    // Setup the widgets
    setWindowIcon(QIcon(":/seer/resources/seergdb_64x64.png"));
    setWindowTitle("Seer Console");

    textEdit->setReadOnly(true);
    textEdit->setTextInteractionFlags(textEdit->textInteractionFlags() | Qt::TextSelectableByKeyboard); // Show cursor
    textEdit->setLineWrapMode(QPlainTextEdit::NoWrap); // No wrap

    // Default font.
    QFont font;
    font.setFamily("monospace");
    font.setStyleHint(QFont::Monospace);
    font.setFixedPitch(true);

    // Set the widget's font.
    QTextCharFormat format = textEdit->currentCharFormat();
    format.setFont(font);
    textEdit->setCurrentCharFormat(format);
    textEdit->setFont(font);

    wrapTextCheckBox->setCheckState(Qt::Unchecked); // No wrap

    // Create psuedo terminal for console.
    createConsole();
    connectConsole();

    // Connect things.
    QObject::connect(clearButton,       &QPushButton::clicked,      this,  &SeerConsoleWidget::handleClearButton);
    QObject::connect(printButton,       &QPushButton::clicked,      this,  &SeerConsoleWidget::handlePrintButton);
    QObject::connect(saveButton,        &QPushButton::clicked,      this,  &SeerConsoleWidget::handleSaveButton);
    QObject::connect(fontButton,        &QPushButton::clicked,      this,  &SeerConsoleWidget::handleFontButton);
    QObject::connect(wrapTextCheckBox,  &QCheckBox::clicked,        this,  &SeerConsoleWidget::handleWrapTextCheckBox);
    QObject::connect(stdoutCheckBox,    &QCheckBox::clicked,        this,  &SeerConsoleWidget::handleStdoutCheckBox);
    QObject::connect(stdinLineEdit,     &QLineEdit::returnPressed,  this,  &SeerConsoleWidget::handleStdinLineEdit);

    // Restore window settings.
    readSettings();
}

SeerConsoleWidget::~SeerConsoleWidget () {
    disconnectConsole();
    deleteConsole();
}

const QString& SeerConsoleWidget::ttyDeviceName () const {
    return _ttyDeviceName;
}

void SeerConsoleWidget::handleText (const char* buffer, int count) {

    if (count < 1) {
        return;
    }

     // Write text to stdout on terminal that started Seer..
    if (isStdoutEnabled()) {
        write (STDOUT_FILENO, buffer, count);
    }

    // Write text to Ansi widget.
    QString str = QString::fromLatin1(buffer, count);

    textEdit->insertAnsiText(str);
    textEdit->ensureCursorVisible();

    return;
}

void SeerConsoleWidget::handleChangeWindowTitle (QString title) {

    if (title == "") {
        setWindowTitle("Seer Console");
    }else{
        setWindowTitle("Seer Console - '" + title + "'");
    }
}

void SeerConsoleWidget::handleTabDetached (QWidget* widget) {

    if (widget != (QWidget*)this) {
        return;
    }

    // Resize the detached console with the size from
    // the settings.
    QSettings settings;

    settings.beginGroup("consolewindow"); {
        resize(settings.value("size", QSize(800, 600)).toSize());
    } settings.endGroup();
}

void SeerConsoleWidget::handleTabReattached (QWidget* widget) {
    // Do nothing for now.
    Q_UNUSED(widget);
}

void SeerConsoleWidget::handleClearButton () {
    textEdit->clear();
}

void SeerConsoleWidget::handlePrintButton () {

    QPrinter printer;

    QPrintDialog* dlg = new QPrintDialog(&printer, this);

    if (dlg->exec() != QDialog::Accepted) {
        return;
    }

    QTextDocument* document = textEdit->document();

    document->print(&printer);
}

void SeerConsoleWidget::handleSaveButton () {

    QFileDialog dialog(this, "Seer console log file", "./", "Logs (*.log);;Text files (*.txt);;All files (*.*)");
    dialog.setOptions(QFileDialog::DontUseNativeDialog);
    dialog.setAcceptMode(QFileDialog::AcceptSave);
    dialog.setFileMode(QFileDialog::AnyFile);
    dialog.setDefaultSuffix("log");
    dialog.selectFile("consoleoutput.log");

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

void SeerConsoleWidget::handleFontButton () {

    // Get current format for the font name.
    bool ok;

    QFont font = QFontDialog::getFont(&ok, textEdit->font().toString(), this, "Seer - Select Console Font", QFontDialog::DontUseNativeDialog|QFontDialog::MonospacedFonts);

    if (ok == false) {
        return;
    }

    // Set the widget's font.
    QTextCharFormat format = textEdit->currentCharFormat();
    format.setFont(font);
    textEdit->setCurrentCharFormat(format);
    textEdit->setFont(font);

    writeFontSettings();
}

void SeerConsoleWidget::handleWrapTextCheckBox () {

    if (wrapTextCheckBox->checkState() == Qt::Unchecked) {
        textEdit->setLineWrapMode(QPlainTextEdit::NoWrap);       // No wrap
        _enableWrap = false;
    }else{
        textEdit->setLineWrapMode(QPlainTextEdit::WidgetWidth);  // Wrap at end of widget
        _enableWrap = true;
    }

    writeSettings();
}

void SeerConsoleWidget::handleStdoutCheckBox () {

    // Don't write text to the terminal's stdout.
    if (stdoutCheckBox->checkState() == Qt::Unchecked) {
        enableStdout(false);
        writeSettings();
        return;
    }

    // If stdout is not valid, don't set it.
    if (fcntl(STDOUT_FILENO, F_GETFD) == -1) {

        QMessageBox::critical(this, tr("Error"), tr("stdout file descriptor is not valid.\nDisabling writing to stdout."));

        enableStdout(false);
        writeSettings();
        return;
    }

    // All good to write to stdout
    enableStdout(true);
    writeSettings();
}

void SeerConsoleWidget::handleStdinLineEdit () {

    QString str = stdinLineEdit->text();

    if (str == "") {
        return;
    }

    stdinLineEdit->clear();

    str += '\n';

    std::string s = str.toStdString();

    if (_ptsFD < 0) {
        return;
    }

    int n = write(_ptsFD, s.c_str(), s.length());

    if (n != (signed long int)s.length()) {
        qWarning() << "Not able to process stdin of" << s.length() << "bytes.";
    }

    fsync(_ptsFD);
}

void SeerConsoleWidget::handleConsoleOutput (int socketfd) {

    if (_ptsFD < 0) {
        return;
    }

    Q_UNUSED(socketfd);

    char buffer[1024];

    while (1) {
        int n = read(_ptsFD, buffer, sizeof(buffer));

        if (n < 0) {
            if (errno == EAGAIN) {
                break;
            }

            if (errno == EIO) {
                // Disconnect console if tty has an I/O error.
                // Can be reconnected later just before gdb restarts it's target program.
                disconnectConsole();
                break;
            }

            break;
        }

        handleText(buffer, n);
        break;
    }
}

void SeerConsoleWidget::createConsole () {

    // Create tty and its permissions.
    _ptsFD = posix_openpt(O_RDWR | O_NOCTTY);

    if (_ptsFD < 0) {
        qDebug() << "Failed to create tty" << strerror(errno);
        return;
    }

    if (grantpt(_ptsFD)) {
        qDebug() << "Failed to grant pt" << strerror(errno);
        ::close(_ptsFD); _ptsFD = -1;
        return;
    }

    if  (unlockpt(_ptsFD)) {
        qDebug() << "Failed to unlock pt" << strerror(errno);
        ::close(_ptsFD); _ptsFD = -1;
        return;
    }

    // Turn off blocking.
    fcntl(_ptsFD, F_SETFL, O_NDELAY);

    // Set window size
    struct winsize term_winsize;

    term_winsize.ws_col = 80;
    term_winsize.ws_row = 20;
    term_winsize.ws_xpixel = 80 * 8;
    term_winsize.ws_ypixel = 20 * 8;

    if (ioctl(_ptsFD, TIOCSWINSZ, &term_winsize) < 0) {
        qDebug() << "ioctl TIOCSWINSZ failed" << strerror(errno);
    }

    // Set controlling
    if (ioctl(_ptsFD, TIOCSCTTY, (char *)0) < 0) {
        // Seems to work even though this fails.
        // qDebug() << "ioctl TIOCSCTTY failed" << strerror(errno);
    }

    // Save the device name.
    _ttyDeviceName = ptsname(_ptsFD);

    // Set maximum blocks to 0 (unlimited).
    // The createConsole can be followed with a setScrollLines() call.
    setScrollLines(0);
}

void SeerConsoleWidget::connectConsole () {

    disconnectConsole();

    if (_ptsFD < 0) {
        return;
    }

    _ptsListener = new QSocketNotifier(_ptsFD, QSocketNotifier::Read);

    QObject::connect(_ptsListener, &QSocketNotifier::activated, this, &SeerConsoleWidget::handleConsoleOutput);
}

void SeerConsoleWidget::disconnectConsole () {

    if (_ptsListener) {

        QObject::disconnect(_ptsListener, &QSocketNotifier::activated, this, &SeerConsoleWidget::handleConsoleOutput);

        delete _ptsListener; _ptsListener = 0;
    }
}

void SeerConsoleWidget::deleteConsole () {

    if (_ptsFD < 0) {
        return;
    }

    _ttyDeviceName = "";

    ::close(_ptsFD); _ptsFD = -1;
}

void SeerConsoleWidget::setScrollLines (int count) {

    textEdit->setMaximumBlockCount(count);
}

int SeerConsoleWidget::scrollLines () const {

    return textEdit->maximumBlockCount();
}

void SeerConsoleWidget::enableStdout (bool flag) {

    _enableStdout = flag;

    stdoutCheckBox->setChecked(_enableStdout);
}

bool SeerConsoleWidget::isStdoutEnabled () const {
    return _enableStdout;
}

void SeerConsoleWidget::enableWrap (bool flag) {

    _enableWrap = flag;

    wrapTextCheckBox->setChecked(_enableWrap);
}

bool SeerConsoleWidget::isWrapEnabled () const {
    return _enableWrap;
}

void SeerConsoleWidget::resetSize () {

    // If there's a parent, don't reset the size.
    // This means the console is attached in the
    // tab bar and its size has been shrunk. We
    // only want to resize if the console
    // has been detached, ie: no parent.
    if (parent() != 0) {
        return;
    }

    QSettings settings;

    settings.beginGroup("consolewindow"); {
        resize(settings.value("size", QSize(800, 600)).toSize());
    } settings.endGroup();
}

void SeerConsoleWidget::writeSettings() {

    QSettings settings;

    settings.beginGroup("consolewindow"); {
        settings.setValue("stdout", isStdoutEnabled());
        settings.setValue("wrap",   isWrapEnabled());
    }settings.endGroup();
}

void SeerConsoleWidget::writeFontSettings() {

    QSettings settings;

    settings.beginGroup("consolewindow"); {
        settings.setValue("font", textEdit->font().toString());
    }settings.endGroup();
}

void SeerConsoleWidget::writeSizeSettings() {

    // If there's a parent, don't save the size.
    // This means the console is attached in the
    // tab bar and its size has been shrunk. We
    // only want to save resizes if the console
    // has been detached, ie: no parent.
    if (parent() != 0) {
        return;
    }

    QSettings settings;

    settings.beginGroup("consolewindow"); {
        settings.setValue("size", size());
    }settings.endGroup();
}

void SeerConsoleWidget::readSettings() {

    QSettings settings;

    settings.beginGroup("consolewindow"); {

        resize(settings.value("size", QSize(800, 600)).toSize());
        enableStdout(settings.value("stdout", false).toBool());
        enableWrap(settings.value("wrap", false).toBool());

        QFont f;
        if (settings.contains("font")) {

            f.fromString(settings.value("font").toString());

        }else{
            f = QFont("monospace", 10);
        }

        // Get current format for the font name.
        QTextCharFormat format = textEdit->currentCharFormat();

        format.setFont(f);

        textEdit->setCurrentCharFormat(format);
        textEdit->setFont(f);

    } settings.endGroup();
}

void SeerConsoleWidget::resizeEvent (QResizeEvent* event) {

    // Write window settings.
    writeSizeSettings();

    QWidget::resizeEvent(event);
}

