#include "SeerSlashProcDialog.h"

SeerSlashProcDialog::SeerSlashProcDialog (QWidget* parent) : QDialog(parent) {

    // Set up the UI.
    setupUi(this);

    // Setup the widgets

    // Connect things.
}

SeerSlashProcDialog::~SeerSlashProcDialog () {
}

int SeerSlashProcDialog::selectedPid () const {

    return slashProcWidget->selectedPid();
}

QString SeerSlashProcDialog::selectedName () const {

    return slashProcWidget->selectedName();
}

QString SeerSlashProcDialog::selectedCommandLine () const {

    return slashProcWidget->selectedCommandLine();
}

