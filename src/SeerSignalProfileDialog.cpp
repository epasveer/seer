// SPDX-FileCopyrightText: 2026 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "SeerSignalProfileDialog.h"
#include <QtWidgets/QMessageBox>
#include <QtWidgets/QFileDialog>
#include <QtGui/QRegularExpressionValidator>
#include <QtCore/QRegularExpression>
#include <QtCore/QSettings>
#include <QtCore/QList>
#include <QtCore/QDebug>

SeerSignalProfileDialog::SeerSignalProfileDialog (QWidget* parent) : QDialog(parent) {

    // Set up the UI.
    setupUi(this);

    signalsTreeWidget->resizeColumnToContents(0); // name
    signalsTreeWidget->resizeColumnToContents(1); // checkbox
    signalsTreeWidget->clear();

    // Letters, numbers, space, period, hypen, underscore.
    QRegularExpressionValidator* validator = new QRegularExpressionValidator(QRegularExpression("[a-zA-Z0-9\\ \\.\\-\\_]+"));

    profileNameLineEdit->setValidator(validator);

    // Setup the widgets
    setSignals(QStringList(), QVector<bool>());

    // Connect things.
    QObject::connect(enablePushButton,   &QPushButton::clicked,          this, &SeerSignalProfileDialog::handleEnableSelected);
    QObject::connect(disablePushButton,  &QPushButton::clicked,          this, &SeerSignalProfileDialog::handleDisableSelected);
    QObject::connect(importPushButton,   &QPushButton::clicked,          this, &SeerSignalProfileDialog::handleImportFile);
    QObject::connect(exportPushButton,   &QPushButton::clicked,          this, &SeerSignalProfileDialog::handleExportFile);

    // Restore window settings.
    readSettings();
}

SeerSignalProfileDialog::~SeerSignalProfileDialog () {
}

void SeerSignalProfileDialog::setSignals (const QStringList& signalNames, const QVector<bool>& signalEnabled) {

    signalsTreeWidget->clear();

    for (int i=0; i<signalNames.count(); i++) {

        QTreeWidgetItem* topItem = new QTreeWidgetItem;
        topItem->setText(0, signalNames[i]);

        if (signalEnabled[i] == true) {
            topItem->setCheckState(1, Qt::Checked);
        }else{
            topItem->setCheckState(1, Qt::Unchecked);
        }

        signalsTreeWidget->addTopLevelItem(topItem);
    }

    signalsTreeWidget->resizeColumnToContents(0);
    signalsTreeWidget->resizeColumnToContents(1);
}

QStringList SeerSignalProfileDialog::signalNames () const {

    QStringList signalNames;

    for (int i=0; i<signalsTreeWidget->topLevelItemCount(); i++) {

        QTreeWidgetItem* topItem = signalsTreeWidget->topLevelItem(i);

        signalNames.push_back(topItem->text(0));
    }

    return signalNames;
}

QVector<bool> SeerSignalProfileDialog::signalEnabled () const {

    QVector<bool> signalEnabled;

    for (int i=0; i<signalsTreeWidget->topLevelItemCount(); i++) {

        QTreeWidgetItem* topItem = signalsTreeWidget->topLevelItem(i);

        if (topItem->checkState(1) == Qt::Checked) {
            signalEnabled.push_back(true);
        }else{
            signalEnabled.push_back(false);
        }
    }

    return signalEnabled;
}

void SeerSignalProfileDialog::setProfileName (const QString& profileName) {

    profileNameLineEdit->setText(profileName);
}

QString SeerSignalProfileDialog::profileName () const {

    return profileNameLineEdit->text();
}

void SeerSignalProfileDialog::accept () {

    if (profileNameLineEdit->text() == "") {
        QMessageBox::warning(this, "Seer", "The signal profile name is blank.", QMessageBox::Ok);
        return;
    }

    if (profileNameLineEdit->text() == "allsignals") {
        QMessageBox::warning(this, "Seer", "The signal profile name of 'allsignals' is reserved.\n\nChoose a different name.", QMessageBox::Ok);
        return;
    }

    QDialog::accept();
}

void SeerSignalProfileDialog::handleEnableSelected () {

     QList<QTreeWidgetItem*> selected = signalsTreeWidget->selectedItems();

     foreach (QTreeWidgetItem* item, selected) {
        item->setCheckState(1, Qt::Checked);
     }
}

void SeerSignalProfileDialog::handleDisableSelected () {

     QList<QTreeWidgetItem*> selected = signalsTreeWidget->selectedItems();

     foreach (QTreeWidgetItem* item, selected) {
        item->setCheckState(1, Qt::Unchecked);
     }
}

void SeerSignalProfileDialog::handleImportFile () {

    //
    // Get the name of the file to import.
    //
    QFileDialog dialog(this, "Seer Signal Profile File", "./", "Text files (*.txt);;All files (*.*)");
    dialog.setAcceptMode(QFileDialog::AcceptOpen);
    dialog.setFileMode(QFileDialog::AnyFile);
    dialog.setDefaultSuffix("txt");
    dialog.selectFile("signalprofile.txt");

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

    //
    // Open the file and read from it.
    // A simple format of:
    //      <signalname>   <enable|disable>
    //
    QFile file(files[0]);

    if (file.open(QIODevice::ReadOnly)) {

        QStringList     signalNames;
        QVector<bool>   signalEnabled;
        QTextStream     stream(&file);

        while (stream.atEnd() == false) {

            QString line = stream.readLine();

            QStringList words = line.split(QRegularExpression("\\s+"));

            // Ignore empty lines.
            if (words.count() == 0) {
                continue;
            }

            // Ignore null lines.
            if (words.count() == 1 && words[0] == "") {
                continue;
            }

            // There should be only two words on each line.
            if (words.count() != 2) {
                QMessageBox::critical(this, tr("Error"), tr("Can't load signal profile file.\nEncountered a line not the form of:\n\n<signalname>   <enabled|disabled>"));
                return;
            }

            // The second word must be 'enabled' or 'disabled'.
            if (words[1] != "enabled" && words[1] != "disabled") {
                QMessageBox::critical(this, tr("Error"), tr("Can't load signal profile file.\nEncountered a line not the form of:\n\n<signalname>   <enabled|disabled>"));
                return;
            }

            signalNames.push_back(words[0]);
            signalEnabled.push_back(words[1] == "enabled" ? true : false);
        }

        // Populate the list with the imported values.
        setSignals (signalNames, signalEnabled);

        QMessageBox::information(this, "Seer", "Loaded.");

    }else{

        QMessageBox::critical(this, tr("Error"), tr("Can't open signal profile file."));
        return;
    }

}

void SeerSignalProfileDialog::handleExportFile () {

    //
    // Get the name of the file to export.
    //
    QFileDialog dialog(this, "Seer Signal Profile File", "./", "Text files (*.txt);;All files (*.*)");
    dialog.setAcceptMode(QFileDialog::AcceptSave);
    dialog.setFileMode(QFileDialog::AnyFile);
    dialog.setDefaultSuffix("txt");
    dialog.selectFile("signalprofile.txt");

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

    //
    // Create the file and write to it.
    // A simple format of:
    //      <signalname>   <enable|disable>
    //
    QFile file(files[0]);

    if (file.open(QIODevice::ReadWrite)) {

        QTextStream stream(&file);

        QStringList   names   = signalNames();
        QVector<bool> enabled = signalEnabled();

        for (int i=0; i<names.count(); i++) {
            stream << names[i] << "    " << (enabled[i] == true ? "enabled" : "disabled") << "\n";
        }

        file.flush();
        file.close();

        QMessageBox::information(this, "Seer", "Saved.");

    }else{

        QMessageBox::critical(this, tr("Error"), tr("Can't create signal profile file."));

        return;
    }
}

void SeerSignalProfileDialog::writeSettings() {

    QSettings settings;

    settings.beginGroup("signalprofiledialog"); {
        settings.setValue("size", size());
    }settings.endGroup();
}

void SeerSignalProfileDialog::readSettings() {

    QSettings settings;

    settings.beginGroup("signalprofiledialog"); {
        resize(settings.value("size", QSize(600, 600)).toSize());
    } settings.endGroup();
}

void SeerSignalProfileDialog::resizeEvent (QResizeEvent* event) {

    // Write window settings.
    writeSettings();

    QWidget::resizeEvent(event);
}

