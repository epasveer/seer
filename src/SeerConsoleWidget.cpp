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

SeerConsoleWidget::SeerConsoleWidget (QWidget* parent) : QWidget(parent) {

    // Init variables.
    _ttyDeviceName = "";
    _ptsFD         = -1;
    _ptsListener   = 0;
    _mode          = "normal";

    // Set up UI.
    setupUi(this);

    // Setup the widgets
    setWindowIcon(QIcon(":/seer/resources/seergdb_64x64.png"));
    setWindowTitle("Seer Console");
    setWindowFlags(Qt::Window | Qt::WindowMinimizeButtonHint | Qt::WindowMaximizeButtonHint);

    QFont font;
    font.setFamily("monospace");
    font.setFixedPitch(true);
    font.setStyleHint(QFont::TypeWriter);

    textEdit->setReadOnly(true);
    textEdit->setTextInteractionFlags(textEdit->textInteractionFlags() | Qt::TextSelectableByKeyboard); // Show cursor
    textEdit->setFont(font);
    textEdit->setLineWrapMode(QPlainTextEdit::NoWrap); // No wrap
    wrapTextCheckBox->setCheckState(Qt::Unchecked); // No wrap

    _cursor = QTextCursor(textEdit->document());

    // Create psuedo terminal for console.
    createConsole();
    connectConsole();

    setMode("normal");

    // Connect things.
    QObject::connect(clearButton,       &QPushButton::clicked,      this,  &SeerConsoleWidget::handleClearButton);
    QObject::connect(printButton,       &QPushButton::clicked,      this,  &SeerConsoleWidget::handlePrintButton);
    QObject::connect(saveButton,        &QPushButton::clicked,      this,  &SeerConsoleWidget::handleSaveButton);
    QObject::connect(fontButton,        &QPushButton::clicked,      this,  &SeerConsoleWidget::handleFontButton);
    QObject::connect(wrapTextCheckBox,  &QCheckBox::clicked,        this,  &SeerConsoleWidget::handleWrapTextCheckBox);
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

    // parse off lines
    const char* start = buffer;

    while (count > 0) {

        int len = 0;

        while (count > 0 && start[len] != '\n' && start[len] != '\r') {
            --count;
            ++len;
        }

        if (len > 0) {
            QString str = QString::fromLatin1(start, len);
            // replace text in the last line
            // this selection is non-empty only after a '\r' that was not
            // followed by a '\n'
            _cursor.movePosition(QTextCursor::Right, QTextCursor::KeepAnchor, len);
            _cursor.insertText(str);
            start += len;
            len = 0;
        }

        if (count > 0 && *start == '\r') {
            ++start;
            --count;
            _cursor.movePosition(QTextCursor::StartOfLine);
        }

        if (count > 0 && *start == '\n') {
            ++start;
            --count;
            _cursor.movePosition(QTextCursor::End);
            _cursor.insertText(QString('\n'));
        }

        textEdit->verticalScrollBar()->setValue(textEdit->verticalScrollBar()->maximum());
    }
}

void SeerConsoleWidget::handleChangeWindowTitle (QString title) {

    if (title == "") {
        setWindowTitle("Seer Console");
    }else{
        setWindowTitle("Seer Console - '" + title + "'");
    }
}

void SeerConsoleWidget::handleClearButton () {
    textEdit->clear();
    _cursor.movePosition(QTextCursor::End);
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

void SeerConsoleWidget::handleFontButton () {

    bool ok;

    QFont font = QFontDialog::getFont(&ok, textEdit->font(), this, "Seer - Select Console Font", QFontDialog::DontUseNativeDialog|QFontDialog::MonospacedFonts);

    if (ok == false) {
        return;
    }

    textEdit->setFont(font);

    writeSettings();
}

void SeerConsoleWidget::handleWrapTextCheckBox () {

    if (wrapTextCheckBox->checkState() == Qt::Unchecked) {
        textEdit->setLineWrapMode(QPlainTextEdit::NoWrap);       // No wrap
    }else{
        textEdit->setLineWrapMode(QPlainTextEdit::WidgetWidth);  // Wrap at end of widget
    }
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

void SeerConsoleWidget::setMode (const QString& mode) {

    //qDebug() << mode;

    if (mode == "normal") {

        _mode = mode;

        show();
        setWindowState(Qt::WindowNoState);

    }else if (mode == "minimized") {

        _mode = mode;

        show();
        setWindowState(Qt::WindowMinimized);

    }else if (mode == "hidden") {

        _mode = mode;

        hide();

    }else if (mode == "") {

        _mode = "normal";

        show();
        setWindowState(Qt::WindowNoState);
    }
}

QString SeerConsoleWidget::mode () const {

    if (_mode == "") {
        return "normal";
    }

    return _mode;
}

void SeerConsoleWidget::writeSettings() {

    QSettings settings;

    settings.beginGroup("consolewindow"); {
        settings.setValue("size", size());
        settings.setValue("font", textEdit->font().toString());
    }settings.endGroup();

    //qDebug() << size();
}

void SeerConsoleWidget::readSettings() {

    QSettings settings;

    settings.beginGroup("consolewindow"); {
        resize(settings.value("size", QSize(800, 600)).toSize());

        QFont f;
        if (settings.contains("font")) {
            f.fromString(settings.value("font").toString());
        }else{
            f = QFont("monospace", 10);
        }

        textEdit->setFont(f);

    } settings.endGroup();

    //qDebug() << size();
}

void SeerConsoleWidget::resizeEvent (QResizeEvent* event) {

    // Write window settings.
    writeSettings();

    QWidget::resizeEvent(event);
}

