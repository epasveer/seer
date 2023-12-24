#include "SeerRegisterProfileDialog.h"
#include "SeerRegisterTreeWidgetItem.h"
#include <QtWidgets/QMessageBox>
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

    QRegularExpressionValidator* validator = new QRegularExpressionValidator(QRegularExpression("[a-z0-9\\.\\-\\_]+"));

    profileNameLineEdit->setValidator(validator);

    // Setup the widgets
    setRegisters(QStringList(), QVector<bool>());

    // Connect things.
    QObject::connect(enablePushButton,   &QPushButton::clicked,          this, &SeerRegisterProfileDialog::handleEnableSelected);
    QObject::connect(disablePushButton,  &QPushButton::clicked,          this, &SeerRegisterProfileDialog::handleDisableSelected);

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

