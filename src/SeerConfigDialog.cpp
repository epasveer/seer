#include "SeerConfigDialog.h"
#include <QtWidgets/QListWidget>
#include <QtWidgets/QListWidgetItem>
#include <QtWidgets/QStackedWidget>
#include <QtWidgets/QPushButton>
#include <QtWidgets/QMessageBox>
#include <QtCore/QString>
#include <QtCore/QStringList>
#include <QtCore/QDebug>

SeerConfigDialog::SeerConfigDialog(QWidget* parent) : QDialog(parent) {

    // Set up the UI.
    setupUi(this);

    // Setup the widgets
    QSize size(96, 84);
    int   spacing = 12;

    contentsListWidget->setViewMode(QListView::IconMode);
    contentsListWidget->setIconSize(size);
    contentsListWidget->setMovement(QListView::Static);
    contentsListWidget->setSpacing(spacing);
    contentsListWidget->setFixedWidth(size.width()+(spacing*3));
    contentsListWidget->setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);

    QPushButton* resetButton = buttonBox->button(QDialogButtonBox::Reset);
    if (resetButton) {
        resetButton->setToolTip("Reset configuration for the current page to initial settings.");
    }

    QPushButton* okButton = buttonBox->button(QDialogButtonBox::Ok);
    if (okButton) {
        okButton->setToolTip("Accept configuration changes to all pages.");
    }

    QPushButton* cancelButton = buttonBox->button(QDialogButtonBox::Cancel);
    if (cancelButton) {
        cancelButton->setToolTip("Discard configuration changes to all pages.");
    }

    // Create pages.
    _seerConfigPage     = new SeerSeerConfigPage;
    _gdbConfigPage      = new SeerGdbConfigPage;
    _editorConfigPage   = new SeerEditorConfigPage;
    _sourceConfigPage   = new SeerSourceConfigPage;
    _assemblyConfigPage = new SeerAssemblyConfigPage;
    _keysConfigPage     = new SeerKeysConfigPage;
    _rrConfigPage       = new SeerRRConfigPage;

    // Add the pages to the stacked widget.
    pagesStackedWidget->addWidget(_seerConfigPage);
    pagesStackedWidget->addWidget(_gdbConfigPage);
    pagesStackedWidget->addWidget(_editorConfigPage);
    pagesStackedWidget->addWidget(_sourceConfigPage);
    pagesStackedWidget->addWidget(_assemblyConfigPage);
    pagesStackedWidget->addWidget(_keysConfigPage);
    pagesStackedWidget->addWidget(_rrConfigPage);

    // Create icons.
    QListWidgetItem* configSeerButton = new QListWidgetItem(contentsListWidget);
    configSeerButton->setIcon(QIcon(":/seer/resources/seergdb_128x128.png"));
    configSeerButton->setText(tr("Seer"));
    configSeerButton->setTextAlignment(Qt::AlignHCenter);
    configSeerButton->setFlags(Qt::ItemIsSelectable | Qt::ItemIsEnabled);

    QListWidgetItem* configGdbButton = new QListWidgetItem(contentsListWidget);
    configGdbButton->setIcon(QIcon(":/seer/resources/icons-icons/gdb.png"));
    configGdbButton->setText(tr("GDB"));
    configGdbButton->setTextAlignment(Qt::AlignHCenter);
    configGdbButton->setFlags(Qt::ItemIsSelectable | Qt::ItemIsEnabled);

    QListWidgetItem* configEditorButton = new QListWidgetItem(contentsListWidget);
    configEditorButton->setIcon(QIcon(":/seer/resources/icons-icons/editor.png"));
    configEditorButton->setText(tr("Editor"));
    configEditorButton->setTextAlignment(Qt::AlignHCenter);
    configEditorButton->setFlags(Qt::ItemIsSelectable | Qt::ItemIsEnabled);

    QListWidgetItem* configSourceButton = new QListWidgetItem(contentsListWidget);
    configSourceButton->setIcon(QIcon(":/seer/resources/thenounproject/source.svg"));
    configSourceButton->setText(tr("Source"));
    configSourceButton->setTextAlignment(Qt::AlignHCenter);
    configSourceButton->setFlags(Qt::ItemIsSelectable | Qt::ItemIsEnabled);

    QListWidgetItem* configAssemblyButton = new QListWidgetItem(contentsListWidget);
    configAssemblyButton->setIcon(QIcon(":/seer/resources/thenounproject/assembly.svg"));
    configAssemblyButton->setText(tr("Assembly"));
    configAssemblyButton->setTextAlignment(Qt::AlignHCenter);
    configAssemblyButton->setFlags(Qt::ItemIsSelectable | Qt::ItemIsEnabled);

    QListWidgetItem* configKeysButton = new QListWidgetItem(contentsListWidget);
    configKeysButton->setIcon(QIcon(":/seer/resources/thenounproject/keyboard.svg"));
    configKeysButton->setText(tr("Keys"));
    configKeysButton->setTextAlignment(Qt::AlignHCenter);
    configKeysButton->setFlags(Qt::ItemIsSelectable | Qt::ItemIsEnabled);

    QListWidgetItem* configRRButton = new QListWidgetItem(contentsListWidget);
    configRRButton->setIcon(QIcon(":/seer/resources/rr_256x256.png"));
    configRRButton->setText(tr("RR"));
    configRRButton->setTextAlignment(Qt::AlignHCenter);
    configRRButton->setFlags(Qt::ItemIsSelectable | Qt::ItemIsEnabled);

    // Connect things.
    connect(contentsListWidget, &QListWidget::currentItemChanged,   this, &SeerConfigDialog::handleChangePage);
    connect(buttonBox,          &QDialogButtonBox::clicked,         this, &SeerConfigDialog::handleButtonClicked);

    // Set to first page.
    contentsListWidget->setCurrentRow(0);
}

SeerConfigDialog::~SeerConfigDialog() {
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

void SeerConfigDialog::setGdbNonStopMode (bool flag) {

    _gdbConfigPage->setGdbNonStopMode(flag);
}

bool SeerConfigDialog::gdbNonStopMode () const {

    return _gdbConfigPage->gdbNonStopMode();
}

void SeerConfigDialog::setGdbHandleTerminatingException (bool flag) {

    _gdbConfigPage->setGdbHandleTerminatingException(flag);
}

bool SeerConfigDialog::gdbHandleTerminatingException () const {

    return _gdbConfigPage->gdbHandleTerminatingException();
}

void SeerConfigDialog::setGdbRandomizeStartAddress (bool flag) {

    _gdbConfigPage->setGdbRandomizeStartAddress(flag);
}

bool SeerConfigDialog::gdbRandomizeStartAddress () const {

    return _gdbConfigPage->gdbRandomizeStartAddress();
}

void SeerConfigDialog::setGdbEnablePrettyPrinting (bool flag) {

    _gdbConfigPage->setGdbEnablePrettyPrinting(flag);
}

bool SeerConfigDialog::gdbEnablePrettyPrinting () const {

    return _gdbConfigPage->gdbEnablePrettyPrinting();
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

void SeerConfigDialog::setEditorTabSize (int spaces) {

    _editorConfigPage->setEditorTabSize(spaces);
}

int SeerConfigDialog::editorTabSize () const {

    return _editorConfigPage->editorTabSize();
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

void SeerConfigDialog::setExternalEditorCommand (const QString& externalEditorCommand) {

    _editorConfigPage->setExternalEditorCommand(externalEditorCommand);
}

QString SeerConfigDialog::externalEditorCommand () const {

    return _editorConfigPage->externalEditorCommand();
}

void SeerConfigDialog::setSourceAlternateDirectories (const QStringList& alternateDirectories) {

    _sourceConfigPage->setAlternateDirectories(alternateDirectories);
}

QStringList SeerConfigDialog::sourceAlternateDirectories () const {

    return _sourceConfigPage->alternateDirectories();
}

void SeerConfigDialog::setSourceIgnoreFilePatterns (const QStringList& filePatterns) {

    _sourceConfigPage->setIgnoreFilePatterns(filePatterns);
}

QStringList SeerConfigDialog::sourceIgnoreFilePatterns () const {

    return _sourceConfigPage->ignoreFilePatterns();
}

void SeerConfigDialog::setSourceMiscFilePatterns (const QStringList& filePatterns) {

    _sourceConfigPage->setMiscFilePatterns(filePatterns);
}

QStringList SeerConfigDialog::sourceMiscFilePatterns () const {

    return _sourceConfigPage->miscFilePatterns();
}

void SeerConfigDialog::setSourceSourceFilePatterns (const QStringList& filePatterns) {

    _sourceConfigPage->setSourceFilePatterns(filePatterns);
}

QStringList SeerConfigDialog::sourceSourceFilePatterns () const {

    return _sourceConfigPage->sourceFilePatterns();
}

void SeerConfigDialog::setSourceHeaderFilePatterns (const QStringList& filePatterns) {

    _sourceConfigPage->setHeaderFilePatterns(filePatterns);
}

QStringList SeerConfigDialog::sourceHeaderFilePatterns () const {

    return _sourceConfigPage->headerFilePatterns();
}

void SeerConfigDialog::setAssemblyShowAssemblyTabOnStartup (bool flag) {

    _assemblyConfigPage->setShowAssemblyTabOnStartup(flag);
}

bool SeerConfigDialog::assemblyShowAssemblyTabOnStartup () const {

    return _assemblyConfigPage->showAssemblyTabOnStartup();
}

void SeerConfigDialog::setAssemblyKeepAssemblyTabOnTop (bool flag) {

    _assemblyConfigPage->setKeepAssemblyTabOnTop(flag);
}

bool SeerConfigDialog::assemblyKeepAssemblyTabOnTop () const {

    return _assemblyConfigPage->keepAssemblyTabOnTop();
}

void SeerConfigDialog::setAssemblyDisassemblyFlavor (const QString& flavor) {

    _assemblyConfigPage->setDisassemblyFlavor(flavor);
}

QString SeerConfigDialog::assemblyDisassemblyFlavor () const {

    return _assemblyConfigPage->disassemblyFlavor();
}

void SeerConfigDialog::setAssemblySymbolDemagling (const QString& onoff) {

    _assemblyConfigPage->setSymbolDemagling(onoff);
}

QString SeerConfigDialog::assemblySymbolDemagling () const {

    return _assemblyConfigPage->symbolDemagling();
}

void SeerConfigDialog::setAssemblyShowAddressColumn (bool flag) {

    _assemblyConfigPage->setShowAddressColumn(flag);
}

bool SeerConfigDialog::assemblyShowAddressColumn () const {

    return _assemblyConfigPage->showAddressColumn();
}

void SeerConfigDialog::setAssemblyShowOffsetColumn (bool flag) {

    _assemblyConfigPage->setShowOffsetColumn(flag);
}

bool SeerConfigDialog::assemblyShowOffsetColumn () const {

    return _assemblyConfigPage->showOffsetColumn();
}

void SeerConfigDialog::setAssemblyShowOpcodeColumn (bool flag) {

    _assemblyConfigPage->setShowOpcodeColumn(flag);
}

bool SeerConfigDialog::assemblyShowOpcodeColumn () const {

    return _assemblyConfigPage->showOpcodeColumn();
}

void SeerConfigDialog::setAssemblyShowSourceLines (bool flag) {

    _assemblyConfigPage->setShowSourceLines(flag);
}

bool SeerConfigDialog::assemblyShowSourceLines () const {

    return _assemblyConfigPage->showSourceLines();
}

void SeerConfigDialog::setAssemblyRegisterFormat (const QString& format) {

    _assemblyConfigPage->setRegisterFormat(format);
}

QString SeerConfigDialog::assemblyRegisterFormat () const {

    return _assemblyConfigPage->registerFormat();
}

void SeerConfigDialog::setAssemblyDisassemblyMode (const QString& mode, int bytes) {

    _assemblyConfigPage->setDisassemblyMode(mode, bytes);
}

QString SeerConfigDialog::assemblyDisassemblyMode () const {

    return _assemblyConfigPage->disassemblyMode();
}

int SeerConfigDialog::assemblyDisassemblyBytes () const {

    return _assemblyConfigPage->disassemblyBytes();
}

void SeerConfigDialog::setKeySettings (const SeerKeySettings& settings) {

    _keysConfigPage->setKeySettings(settings);
}

SeerKeySettings SeerConfigDialog::keySettings () const {

    return _keysConfigPage->keySettings();
}

void SeerConfigDialog::setRRProgram (const QString& program) {

    _rrConfigPage->setRRProgram(program);
}

QString SeerConfigDialog::rrProgram () const {

    return _rrConfigPage->rrProgram();
}

void SeerConfigDialog::setRRArguments (const QString& arguments) {

    _rrConfigPage->setRRArguments(arguments);
}

QString SeerConfigDialog::rrArguments () const {

    return _rrConfigPage->rrArguments();
}

void SeerConfigDialog::setRRGdbArguments (const QString& arguments) {

    _rrConfigPage->setGdbArguments(arguments);
}

QString SeerConfigDialog::rrGdbArguments () const {

    return _rrConfigPage->gdbArguments();
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
                                      QString("Reset settings for the '") + itemLabel + "' tab?",
                                      QMessageBox::Ok|QMessageBox::Cancel, QMessageBox::Cancel);

        if (result != QMessageBox::Ok) {
            return;
        }

        if (itemLabel == "GDB") {

            _gdbConfigPage->reset();

        }else if (itemLabel == "Editor") {

            _editorConfigPage->reset();

        }else if (itemLabel == "Source") {

            _sourceConfigPage->reset();

        }else if (itemLabel == "Assembly") {

            _assemblyConfigPage->reset();

        }else if (itemLabel == "Keys") {

            _keysConfigPage->reset();

        }else if (itemLabel == "Seer") {

            _seerConfigPage->reset();

        }else if (itemLabel == "RR") {

            _rrConfigPage->reset();
        }
    }
}

