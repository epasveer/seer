#include "SeerCloseSourceDialog.h"
#include <QtWidgets/QDialog>
#include <QtWidgets/QWidget>
#include <QtCore/QSettings>
#include <QtCore/QDebug>

SeerCloseSourceDialog::SeerCloseSourceDialog(QWidget* parent) : QDialog(parent) {

    // Set up the UI.
    setupUi(this);

    // Setup the widgets

    // Connect things.

    // Restore window settings.
    readSettings();
}

SeerCloseSourceDialog::~SeerCloseSourceDialog() {
}

void SeerCloseSourceDialog::setFiles (const SeerEditorManagerFiles& files) {

    for (int i=0; i<files.size(); i++) {

        QTreeWidgetItem* topItem = new QTreeWidgetItem;
        topItem->setText(0, files[i].file);
        topItem->setText(1, files[i].fullname);

        filenamesTreeWidget->addTopLevelItem(topItem);
    }

    filenamesTreeWidget->resizeColumnToContents(0);
    filenamesTreeWidget->resizeColumnToContents(1);
}

SeerEditorManagerFiles SeerCloseSourceDialog::files () const {

    SeerEditorManagerFiles files;

    QList<QTreeWidgetItem*> items = filenamesTreeWidget->findItems(QString("*"), Qt::MatchWrap|Qt::MatchWildcard|Qt::MatchRecursive);

    QList<QTreeWidgetItem*>::iterator i;
    for (i = items.begin(); i != items.end(); ++i) {

        SeerEditorManagerFile f;

        f.file     = (*i)->text(0);
        f.fullname = (*i)->text(1);

        files.push_back(f);
    }

    return files;
}

SeerEditorManagerFiles SeerCloseSourceDialog::selectedFiles () const {

    SeerEditorManagerFiles files;

    QList<QTreeWidgetItem*> items = filenamesTreeWidget->selectedItems();

    QList<QTreeWidgetItem*>::iterator i;
    for (i = items.begin(); i != items.end(); ++i) {

        SeerEditorManagerFile f;

        f.file     = (*i)->text(0);
        f.fullname = (*i)->text(1);

        files.push_back(f);
    }

    return files;
}

void SeerCloseSourceDialog::writeSettings() {

    QSettings settings;

    settings.beginGroup("closesourcedialog"); {
        settings.setValue("size", size());
    } settings.endGroup();

    //qDebug() << size();
}

void SeerCloseSourceDialog::readSettings() {

    QSettings settings;

    settings.beginGroup("closesourcedialog"); {
        resize(settings.value("size", QSize(400, 300)).toSize());
    } settings.endGroup();

    //qDebug() << size();
}

void SeerCloseSourceDialog::resizeEvent (QResizeEvent* event) {

    writeSettings();

    QWidget::resizeEvent(event);
}

