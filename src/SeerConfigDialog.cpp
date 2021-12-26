#include "SeerConfigDialog.h"
#include <QtWidgets/QListWidget>
#include <QtWidgets/QListWidgetItem>
#include <QtWidgets/QStackedWidget>
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
    _seerConfigPage   = new SeerSeerConfigPage;

    // Add the pages to the stacked widget.
    pagesStackedWidget->addWidget(_gdbConfigPage);
    pagesStackedWidget->addWidget(_editorConfigPage);
    pagesStackedWidget->addWidget(_sourceConfigPage);
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

    QListWidgetItem* configSeerButton = new QListWidgetItem(contentsListWidget);
    configSeerButton->setIcon(QIcon(":/seer/resources/seer_128x128.png"));
    configSeerButton->setText(tr("Seer"));
    configSeerButton->setTextAlignment(Qt::AlignHCenter);
    configSeerButton->setFlags(Qt::ItemIsSelectable | Qt::ItemIsEnabled);

    // Connect things.
    connect(contentsListWidget, &QListWidget::currentItemChanged,   this, &SeerConfigDialog::changePage);

    // Set to first page.
    contentsListWidget->setCurrentRow(0);
}

SeerConfigDialog::~SeerConfigDialog() {
}

void SeerConfigDialog::changePage(QListWidgetItem* current, QListWidgetItem* previous) {

    //qDebug() << __PRETTY_FUNCTION__ << ":" << current << previous;

    if (!current) {
        current = previous;
    }

    pagesStackedWidget->setCurrentIndex(contentsListWidget->row(current));
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

void SeerConfigDialog::setEditorFont (const QFont& font) {

    _editorConfigPage->setEditorFont(font);
}

const QFont& SeerConfigDialog::editorFont () const {

    return _editorConfigPage->editorFont();
}

