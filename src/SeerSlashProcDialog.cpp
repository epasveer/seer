#include "SeerSlashProcDialog.h"

SeerSlashProcDialog::SeerSlashProcDialog (QWidget* parent) : QDialog(parent) {

    // Set up the UI.
    setupUi(this);

    // Setup the widgets

    // Connect things.
    QObject::connect(processInfoWidget, &QProcessInfoWidget::pidSelected,      this, &SeerSlashProcDialog::handlePidSelected);
}

SeerSlashProcDialog::~SeerSlashProcDialog () {
}

int SeerSlashProcDialog::selectedPid () const {

    return processInfoWidget->selectedPid();
}

QString SeerSlashProcDialog::selectedName () const {

    return processInfoWidget->selectedName();
}

QString SeerSlashProcDialog::selectedCommandLine () const {

    return processInfoWidget->selectedCommandLine();
}

void SeerSlashProcDialog::handlePidSelected() {

    done(QDialog::Accepted);
}

