#include "SeerConfigDialog.h"
#include <QtWidgets/QListWidget>
#include <QtWidgets/QListWidgetItem>
#include <QtWidgets/QStackedWidget>
#include <QtWidgets/QMessageBox>
#include <QtCore/QString>
#include <QtCore/QStringList>
#include <QtCore/QDebug>

SeerConfigDialog::SeerConfigDialog(QWidget* parent) : QDialog(parent) {

    // Set up the UI.
    setupUi(this);

    // Setup the widgets
    contentsListWidget->setViewMode(QListView::IconMode);
    contentsListWidget->setIconSize(QSize(96, 84));
    contentsListWidget->setMovement(QListView::Static);
    contentsListWidget->setMaximumWidth(128);
    contentsListWidget->setSpacing(12);

    // Create pages.
    _gdbConfigPage    = new SeerGdbConfigPage;
    _editorConfigPage = new SeerEditorConfigPage;
    _sourceConfigPage = new SeerSourceConfigPage;
    _keysConfigPage   = new SeerKeysConfigPage;
    _seerConfigPage   = new SeerSeerConfigPage;

    // Add the pages to the stacked widget.
    pagesStackedWidget->addWidget(_gdbConfigPage);
    pagesStackedWidget->addWidget(_editorConfigPage);
    pagesStackedWidget->addWidget(_sourceConfigPage);
    pagesStackedWidget->addWidget(_keysConfigPage);
    pagesStackedWidget->addWidget(_seerConfigPage);

    // Create icons.
    QListWidgetItem* configGdbButton = new QListWidgetItem(contentsListWidget);
    configGdbButton->setIcon(QIcon(":/seer/resources/gdb.png"));
    configGdbButton->setText(tr("GDB"));
    configGdbButton->setTextAlignment(Qt::AlignHCenter);
    configGdbButton->setFlags(Qt::ItemIsSelectable | Qt::ItemIsEnabled);

    QListWidgetItem* configEditorButton = new QListWidgetItem(contentsListWidget);
    configEditorButton->setIcon(QIcon(":/seer/resources/editor.png"));
    configEditorButton->setText(tr("Editor"));
    configEditorButton->setTextAlignment(Qt::AlignHCenter);
    configEditorButton->setFlags(Qt::ItemIsSelectable | Qt::ItemIsEnabled);

    QListWidgetItem* configSourceButton = new QListWidgetItem(contentsListWidget);
    configSourceButton->setIcon(QIcon(":/seer/resources/source.png"));
    configSourceButton->setText(tr("Source"));
    configSourceButton->setTextAlignment(Qt::AlignHCenter);
    configSourceButton->setFlags(Qt::ItemIsSelectable | Qt::ItemIsEnabled);

    QListWidgetItem* configKeysButton = new QListWidgetItem(contentsListWidget);
    configKeysButton->setIcon(QIcon(":/seer/resources/keyboard-key.png"));
    configKeysButton->setText(tr("Keys"));
    configKeysButton->setTextAlignment(Qt::AlignHCenter);
    configKeysButton->setFlags(Qt::ItemIsSelectable | Qt::ItemIsEnabled);

    QListWidgetItem* configSeerButton = new QListWidgetItem(contentsListWidget);
    configSeerButton->setIcon(QIcon(":/seer/resources/seer_128x128.png"));
    configSeerButton->setText(tr("Seer"));
    configSeerButton->setTextAlignment(Qt::AlignHCenter);
    configSeerButton->setFlags(Qt::ItemIsSelectable | Qt::ItemIsEnabled);

    // Connect things.
    connect(contentsListWidget, &QListWidget::currentItemChanged,   this, &SeerConfigDialog::handleChangePage);
    connect(buttonBox,          &QDialogButtonBox::clicked,         this, &SeerConfigDialog::handleButtonClicked);

    // Set to first page.
    contentsListWidget->setCurrentRow(0);
}

SeerConfigDialog::~SeerConfigDialog() {
}

void SeerConfigDialog::setGdbProgram (const QString& program) {

    _gdbConfigPage->setGdbProgram(program);
}

QString SeerConfigDialog::gdbProgram () const {

    return _gdbConfigPage->gdbProgram();
}

void SeerConfigDialog::setGdbArguments (const QString& arguments) {

    _gdbConfigPage->setGdbArguments(arguments);
}

QString SeerConfigDialog::gdbArguments () const {

    return _gdbConfigPage->gdbArguments();
}

void SeerConfigDialog::setGdbAsyncMode (bool flag) {

    _gdbConfigPage->setGdbAsyncMode(flag);
}

bool SeerConfigDialog::gdbAsyncMode () const {

    return _gdbConfigPage->gdbAsyncMode();
}

void SeerConfigDialog::setGdbHandleTerminatingException (bool flag) {

    _gdbConfigPage->setGdbHandleTerminatingException(flag);
}

bool SeerConfigDialog::gdbHandleTerminatingException () const {

    return _gdbConfigPage->gdbHandleTerminatingException();
}

void SeerConfigDialog::setDprintfStyle (const QString& style) {

    _gdbConfigPage->setDprintfStyle(style);
}

QString SeerConfigDialog::dprintfStyle () const {

    return _gdbConfigPage->dprintfStyle();
}

void SeerConfigDialog::setDprintfFunction (const QString& function) {

    _gdbConfigPage->setDprintfFunction(function);
}

QString SeerConfigDialog::dprintfFunction () const {

    return _gdbConfigPage->dprintfFunction();
}

void SeerConfigDialog::setDprintfChannel (const QString& channel) {

    _gdbConfigPage->setDprintfChannel(channel);
}

QString SeerConfigDialog::dprintfChannel () const {

    return _gdbConfigPage->dprintfChannel();
}

void SeerConfigDialog::setEditorFont (const QFont& font) {

    _editorConfigPage->setEditorFont(font);
}

const QFont& SeerConfigDialog::editorFont () const {

    return _editorConfigPage->editorFont();
}

void SeerConfigDialog::setEditorHighlighterSettings (const SeerHighlighterSettings& settings) {

    _editorConfigPage->setHighlighterSettings(settings);
}

const SeerHighlighterSettings& SeerConfigDialog::editorHighlighterSettings () const {

    return _editorConfigPage->highlighterSettings();
}

void SeerConfigDialog::setEditorHighlighterEnabled (bool flag) {

    _editorConfigPage->setHighlighterEnabled(flag);
}

bool SeerConfigDialog::editorHighlighterEnabled () const {

    return _editorConfigPage->highlighterEnabled();
}

void SeerConfigDialog::setSourceAlternateDirectories (const QStringList& alternateDirectories) {

    _sourceConfigPage->setAlternateDirectories(alternateDirectories);
}

QStringList SeerConfigDialog::sourceAlternateDirectories () const {

    return _sourceConfigPage->alternateDirectories();
}

void SeerConfigDialog::setKeySettings (const SeerKeySettings& settings) {

    _keysConfigPage->setKeySettings(settings);
}

SeerKeySettings SeerConfigDialog::keySettings () const {

    return _keysConfigPage->keySettings();
}

void SeerConfigDialog::setSeerConsoleMode (const QString& mode) {

    _seerConfigPage->setConsoleMode(mode);
}

QString SeerConfigDialog::seerConsoleMode () const {

    return _seerConfigPage->consoleMode();
}

void SeerConfigDialog::setSeerConsoleScrollLines (int count) {

    _seerConfigPage->setConsoleScrollLines(count);
}

int SeerConfigDialog::seerConsoleScrollLines () const {

    return _seerConfigPage->consoleScrollLines();
}
void SeerConfigDialog::setSeerRememberWindowSizes (bool flag) {

    _seerConfigPage->setRememberWindowSizes(flag);
}

bool SeerConfigDialog::seerRememberWindowSizes () const {

    return _seerConfigPage->rememberWindowSizes();
}

void SeerConfigDialog::setSeerRememberManualCommandCount (int count) {

    _seerConfigPage->setRememberManualCommandCount(count);
}

int SeerConfigDialog::seerRememberManualCommandCount () const {

    return _seerConfigPage->rememberManualCommandCount();
}

void SeerConfigDialog::setSeerClearManualCommandHistory (bool flag) {

    _seerConfigPage->setClearManualCommandHistory(flag);
}

bool SeerConfigDialog::seerClearManualCommandHistory () const {

    return _seerConfigPage->clearManualCommandHistory();
}

void SeerConfigDialog::handleChangePage(QListWidgetItem* current, QListWidgetItem* previous) {

    //qDebug() << current << previous;

    if (!current) {
        current = previous;
    }

    pagesStackedWidget->setCurrentIndex(contentsListWidget->row(current));
}

void SeerConfigDialog::handleButtonClicked (QAbstractButton* button) {

    // Handle resetting the config pages if "Reset" is clicked.
    if (buttonBox->buttonRole(button) == QDialogButtonBox::ResetRole) {

        QString itemLabel = contentsListWidget->currentItem()->text();

        int result = QMessageBox::warning(this, "Seer",
                                      QString("Reset settings for '") + itemLabel + "'?",
                                      QMessageBox::Ok|QMessageBox::Cancel, QMessageBox::Cancel);

        if (result != QMessageBox::Ok) {
            return;
        }

        if (itemLabel == "GDB") {

            setGdbProgram("/usr/bin/gdb");
            setGdbArguments("--interpreter=mi");
            setGdbAsyncMode(true);
            setGdbHandleTerminatingException(true);

            setDprintfStyle("gdb");
            setDprintfFunction("printf");
            setDprintfChannel("");

        }else if (itemLabel == "Editor") {

            setEditorFont(QFont("Source Code Pro", 10));
            setEditorHighlighterSettings(SeerHighlighterSettings::populateForCPP());
            setEditorHighlighterEnabled(true);

        }else if (itemLabel == "Source") {

            QStringList alternateDirectories;

            alternateDirectories << "./";

            setSourceAlternateDirectories(alternateDirectories);

        }else if (itemLabel == "Keys") {

            setKeySettings(SeerKeySettings::populate());

        }else if (itemLabel == "Seer") {

            setSeerConsoleMode("normal");
            setSeerConsoleScrollLines(1000);
            setSeerRememberWindowSizes(true);
            setSeerRememberManualCommandCount(10);
            setSeerClearManualCommandHistory(false);
        }
    }
}

