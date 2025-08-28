// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "SeerRegisterProfileDialog.h"
#include "SeerRegisterTreeWidgetItem.h"
#include <QtWidgets/QMessageBox>
#include <QtWidgets/QFileDialog>
#include <QtGui/QRegularExpressionValidator>
#include <QtCore/QRegularExpression>
#include <QtCore/QSettings>
#include <QtCore/QList>
#include <QtCore/QDebug>

SeerRegisterProfileDialog::SeerRegisterProfileDialog (QWidget* parent) : QDialog(parent) {

    // Set up the UI.
    setupUi(this);

    registersTreeWidget->setSortingEnabled(true);  // We can sort on columns.
    registersTreeWidget->sortByColumn(0, Qt::AscendingOrder);
    registersTreeWidget->resizeColumnToContents(0); // index
    registersTreeWidget->resizeColumnToContents(1); // name
    registersTreeWidget->resizeColumnToContents(2); // checkbox
    registersTreeWidget->clear();

    // Letters, numbers, space, period, hypen, underscore.
    QRegularExpressionValidator* validator = new QRegularExpressionValidator(QRegularExpression("[a-zA-Z0-9\\ \\.\\-\\_]+"));

    profileNameLineEdit->setValidator(validator);

    // Setup the widgets
    setRegisters(QStringList(), QVector<bool>());

    // Connect things.
    QObject::connect(enablePushButton,   &QPushButton::clicked,          this, &SeerRegisterProfileDialog::handleEnableSelected);
    QObject::connect(disablePushButton,  &QPushButton::clicked,          this, &SeerRegisterProfileDialog::handleDisableSelected);
    QObject::connect(importPushButton,   &QPushButton::clicked,          this, &SeerRegisterProfileDialog::handleImportFile);
    QObject::connect(exportPushButton,   &QPushButton::clicked,          this, &SeerRegisterProfileDialog::handleExportFile);

    // Restore window settings.
    readSettings();
}

SeerRegisterProfileDialog::~SeerRegisterProfileDialog () {
}

void SeerRegisterProfileDialog::setRegisters (const QStringList& registerNames, const QVector<bool>& registerEnabled) {

    registersTreeWidget->clear();

    for (int i=0; i<registerNames.count(); i++) {

        QTreeWidgetItem* topItem = new SeerRegisterTreeWidgetItem;
        topItem->setText(0, QString::number(i));
        topItem->setText(1, registerNames[i]);

        if (registerEnabled[i] == true) {
            topItem->setCheckState(2, Qt::Checked);
        }else{
            topItem->setCheckState(2, Qt::Unchecked);
        }

        registersTreeWidget->addTopLevelItem(topItem);
    }

    registersTreeWidget->resizeColumnToContents(0);
    registersTreeWidget->resizeColumnToContents(1);
    registersTreeWidget->resizeColumnToContents(2);
}

QStringList SeerRegisterProfileDialog::registerNames () const {

    QStringList registerNames;

    for (int i=0; i<registersTreeWidget->topLevelItemCount(); i++) {

        QTreeWidgetItem* topItem = registersTreeWidget->topLevelItem(i);

        registerNames.push_back(topItem->text(1));
    }

    return registerNames;
}

QVector<bool> SeerRegisterProfileDialog::registerEnabled () const {

    QVector<bool> registerEnabled;

    for (int i=0; i<registersTreeWidget->topLevelItemCount(); i++) {

        QTreeWidgetItem* topItem = registersTreeWidget->topLevelItem(i);

        if (topItem->checkState(2) == Qt::Checked) {
            registerEnabled.push_back(true);
        }else{
            registerEnabled.push_back(false);
        }
    }

    return registerEnabled;
}

void SeerRegisterProfileDialog::setProfileName (const QString& profileName) {

    profileNameLineEdit->setText(profileName);
}

QString SeerRegisterProfileDialog::profileName () const {

    return profileNameLineEdit->text();
}

void SeerRegisterProfileDialog::accept () {

    if (profileNameLineEdit->text() == "") {
        QMessageBox::warning(this, "Seer", "The register profile name is blank.", QMessageBox::Ok);
        return;
    }

    if (profileNameLineEdit->text() == "allregisters") {
        QMessageBox::warning(this, "Seer", "The register profile name of 'allregisters' is reserved.\n\nChoose a different name.", QMessageBox::Ok);
        return;
    }

    QDialog::accept();
}

void SeerRegisterProfileDialog::handleEnableSelected () {

     QList<QTreeWidgetItem*> selected = registersTreeWidget->selectedItems();

     foreach (QTreeWidgetItem* item, selected) {
        item->setCheckState(2, Qt::Checked);
     }
}

void SeerRegisterProfileDialog::handleDisableSelected () {

     QList<QTreeWidgetItem*> selected = registersTreeWidget->selectedItems();

     foreach (QTreeWidgetItem* item, selected) {
        item->setCheckState(2, Qt::Unchecked);
     }
}

void SeerRegisterProfileDialog::handleImportFile () {

    //
    // Get the name of the file to import.
    //
    QFileDialog dialog(this, "Seer Register Profile File", "./", "Text files (*.txt);;All files (*.*)");
    dialog.setAcceptMode(QFileDialog::AcceptOpen);
    dialog.setFileMode(QFileDialog::AnyFile);
    dialog.setDefaultSuffix("txt");
    dialog.selectFile("registerprofile.txt");

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
    //      <registername>   <enable|disable>
    //
    QFile file(files[0]);

    if (file.open(QIODevice::ReadOnly)) {

        QStringList     registerNames;
        QVector<bool>   registerEnabled;
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
                QMessageBox::critical(this, tr("Error"), tr("Can't load register profile file.\nEncountered a line not the form of:\n\n<registername>   <enabled|disabled>"));
                return;
            }

            // The second word must be 'enabled' or 'disabled'.
            if (words[1] != "enabled" && words[1] != "disabled") {
                QMessageBox::critical(this, tr("Error"), tr("Can't load register profile file.\nEncountered a line not the form of:\n\n<registername>   <enabled|disabled>"));
                return;
            }

            registerNames.push_back(words[0]);
            registerEnabled.push_back(words[1] == "enabled" ? true : false);
        }

        // Populate the list with the imported values.
        setRegisters (registerNames, registerEnabled);

        QMessageBox::information(this, "Seer", "Loaded.");

    }else{

        QMessageBox::critical(this, tr("Error"), tr("Can't open register profile file."));
        return;
    }

}

void SeerRegisterProfileDialog::handleExportFile () {

    //
    // Get the name of the file to export.
    //
    QFileDialog dialog(this, "Seer Register Profile File", "./", "Text files (*.txt);;All files (*.*)");
    dialog.setAcceptMode(QFileDialog::AcceptSave);
    dialog.setFileMode(QFileDialog::AnyFile);
    dialog.setDefaultSuffix("txt");
    dialog.selectFile("registerprofile.txt");

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
    //      <registername>   <enable|disable>
    //
    QFile file(files[0]);

    if (file.open(QIODevice::ReadWrite)) {

        QTextStream stream(&file);

        QStringList   names   = registerNames();
        QVector<bool> enabled = registerEnabled();

        for (int i=0; i<names.count(); i++) {
            stream << names[i] << "    " << (enabled[i] == true ? "enabled" : "disabled") << "\n";
        }

        file.flush();
        file.close();

        QMessageBox::information(this, "Seer", "Saved.");

    }else{

        QMessageBox::critical(this, tr("Error"), tr("Can't create register profile file."));

        return;
    }
}

void SeerRegisterProfileDialog::writeSettings() {

    QSettings settings;

    settings.beginGroup("registerprofiledialog"); {
        settings.setValue("size", size());
    }settings.endGroup();
}

void SeerRegisterProfileDialog::readSettings() {

    QSettings settings;

    settings.beginGroup("registerprofiledialog"); {
        resize(settings.value("size", QSize(600, 600)).toSize());
    } settings.endGroup();
}

void SeerRegisterProfileDialog::resizeEvent (QResizeEvent* event) {

    // Write window settings.
    writeSettings();

    QWidget::resizeEvent(event);
}

