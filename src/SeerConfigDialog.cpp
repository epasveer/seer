#include <QtWidgets/QListWidget>
#include <QtWidgets/QListWidgetItem>
#include <QtWidgets/QStackedWidget>
#include <QtCore/QDebug>

#include "SeerConfigDialog.h"
#include "SeerGdbConfigPage.h"
#include "SeerEditorConfigPage.h"

SeerConfigDialog::SeerConfigDialog(QWidget* parent) : QDialog(parent) {

    // Set up the UI.
    setupUi(this);

    // Setup the widgets
    contentsListWidget->setViewMode(QListView::IconMode);
    contentsListWidget->setIconSize(QSize(96, 84));
    contentsListWidget->setMovement(QListView::Static);
    contentsListWidget->setMaximumWidth(128);
    contentsListWidget->setSpacing(12);

    //pagesStackedWidget->addWidget(new SeerGdbConfigPage);
    //pagesStackedWidget->addWidget(new UpdatePage);
    //pagesStackedWidget->addWidget(new QueryPage);
    SeerGdbConfigPage* gdbConfigPage = dynamic_cast<SeerGdbConfigPage*>(pagesStackedWidget->widget(0));
    gdbConfigPage->gdbProgramLineEdit->setText("/usr/bin/gdb");
    gdbConfigPage->gdbArgumentsLineEdit->setText("--interpreter=mi");

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

    // Set to first page.
    contentsListWidget->setCurrentRow(0);

    // Connect things.
    connect(contentsListWidget, &QListWidget::currentItemChanged,   this, &SeerConfigDialog::changePage);
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

