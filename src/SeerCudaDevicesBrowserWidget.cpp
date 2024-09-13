#include "SeerCudaDevicesBrowserWidget.h"
#include "SeerUtl.h"
#include <QtWidgets/QTreeWidget>
#include <QtWidgets/QTreeWidgetItemIterator>
#include <QtWidgets/QApplication>
#include <QtCore/QFileInfo>
#include <QtCore/QDebug>

SeerCudaDevicesBrowserWidget::SeerCudaDevicesBrowserWidget (QWidget* parent) : QWidget(parent) {

    // Construct the UI.
    setupUi(this);

    // Setup the widgets
    cudaDevicesTreeWidget->setMouseTracking(true);
    cudaDevicesTreeWidget->setSortingEnabled(false);
    cudaDevicesTreeWidget->resizeColumnToContents(0);  // current
    cudaDevicesTreeWidget->resizeColumnToContents(1);  // device
    cudaDevicesTreeWidget->resizeColumnToContents(2);  // pci bus/dev id
    cudaDevicesTreeWidget->resizeColumnToContents(3);  // name
    cudaDevicesTreeWidget->resizeColumnToContents(4);  // description
    cudaDevicesTreeWidget->resizeColumnToContents(5);  // SM type
    cudaDevicesTreeWidget->resizeColumnToContents(6);  // SMs
    cudaDevicesTreeWidget->resizeColumnToContents(7);  // warps/SM
    cudaDevicesTreeWidget->resizeColumnToContents(8);  // lanes/warp
    cudaDevicesTreeWidget->resizeColumnToContents(9);  // max regs/lane
    cudaDevicesTreeWidget->resizeColumnToContents(10); // active SMs mask

    cudaDevicesTreeWidget->clear();

    // Connect things.
    QObject::connect(cudaDevicesTreeWidget,  &QTreeWidget::itemClicked,          this, &SeerCudaDevicesBrowserWidget::handleItemClicked);
}

SeerCudaDevicesBrowserWidget::~SeerCudaDevicesBrowserWidget () {
}

void SeerCudaDevicesBrowserWidget::handleText (const QString& text) {

    // Don't do any work if the widget is hidden.
    if (isHidden()) {
        return;
    }

    QApplication::setOverrideCursor(Qt::BusyCursor);

    if (text.startsWith("^done,tasks={")) {

        // -cuda-info-devices

        // ^done,InfoCudaDevicesTable={
        //                     nr_rows="0",nr_cols="11",
        //                     hdr=[{width="1",alignment="1",col_name="current",colhdr=" "},
        //                          {width="3",alignment="1",col_name="device",colhdr="Dev"},
        //                          {width="14",alignment="1",col_name="pci_bus",colhdr="PCI Bus/Dev ID"},
        //                          {width="4",alignment="1",col_name="name",colhdr="Name"},
        //                          {width="11",alignment="1",col_name="description",colhdr="Description"},
        //                          {width="7",alignment="1",col_name="sm_type",colhdr="SM Type"},
        //                          {width="3",alignment="1",col_name="num_sms",colhdr="SMs"},
        //                          {width="8",alignment="1",col_name="num_warps",colhdr="Warps/SM"},
        //                          {width="10",alignment="1",col_name="num_lanes",colhdr="Lanes/Warp"},
        //                          {width="13",alignment="1",col_name="num_regs",colhdr="Max Regs/Lane"},
        //                          {width="15",alignment="1",col_name="active_sms_mask",colhdr="Active SMs Mask"}
        //                     ],
        //                     body=[]}

        //qDebug() << text;

        cudaDevicesTreeWidget->clear();

        /* XXX Implement parsing code.
        QString body_text = Seer::parseFirst(text, "body=", '[', ']', false);

        //qDebug() << threads_text;

        if (body_text != "") {

            QStringList tasks_list = Seer::parse(body_text, "", '{', '}', false);

            for ( const auto& task_text : tasks_list ) {

                QString current_text  = Seer::parseFirst(task_text, "current=",   '"', '"', false);
                QString id_text       = Seer::parseFirst(task_text, "id=",        '"', '"', false);
                QString taskid_text   = Seer::parseFirst(task_text, "task-id=",   '"', '"', false);
                QString threadid_text = Seer::parseFirst(task_text, "thread-id=", '"', '"', false);
                QString parentid_text = Seer::parseFirst(task_text, "parent-id=", '"', '"', false);
                QString priority_text = Seer::parseFirst(task_text, "priority=",  '"', '"', false);
                QString state_text    = Seer::parseFirst(task_text, "state=",     '"', '"', false);
                QString name_text     = Seer::parseFirst(task_text, "name=",      '"', '"', false);

                // Create the item.
                QTreeWidgetItem* item = new QTreeWidgetItem;
                item->setText(0, current_text);
                item->setText(1, id_text);
                item->setText(2, taskid_text);
                item->setText(3, threadid_text);
                item->setText(4, parentid_text);
                item->setText(5, priority_text);
                item->setText(6, state_text);
                item->setText(7, name_text);

                // Add the frame to the tree.
                cudaDevicesTreeWidget->addTopLevelItem(item);
            }
        }

        // Clear the selection and select the one for the current thread-id.
        cudaDevicesTreeWidget->clearSelection();

        QList<QTreeWidgetItem*> matches = cudaDevicesTreeWidget->findItems("*", Qt::MatchExactly, 0);
        if (matches.size() > 0) {
            cudaDevicesTreeWidget->setCurrentItem(matches.first());
        }
        */

    }else if (text.startsWith("^error,msg=\"No registers.\"")) {
        cudaDevicesTreeWidget->clear();

    }else{
        // Ignore others.
    }

    cudaDevicesTreeWidget->resizeColumnToContents(0);
    cudaDevicesTreeWidget->resizeColumnToContents(1);
    cudaDevicesTreeWidget->resizeColumnToContents(2);
    cudaDevicesTreeWidget->resizeColumnToContents(3);
    cudaDevicesTreeWidget->resizeColumnToContents(4);
    cudaDevicesTreeWidget->resizeColumnToContents(5);
    cudaDevicesTreeWidget->resizeColumnToContents(6);
    cudaDevicesTreeWidget->resizeColumnToContents(7);
    cudaDevicesTreeWidget->resizeColumnToContents(8);
    cudaDevicesTreeWidget->resizeColumnToContents(9);
    cudaDevicesTreeWidget->resizeColumnToContents(10);

    QApplication::restoreOverrideCursor();
}

void SeerCudaDevicesBrowserWidget::handleStoppingPointReached () {

    // Don't do any work if the widget is hidden.
    if (isHidden()) {
        return;
    }

    refresh();
}

void SeerCudaDevicesBrowserWidget::handleItemClicked (QTreeWidgetItem* item, int column) {

    Q_UNUSED(column);

    emit selectedCudaDevice(item->text(3).toInt());
}

void SeerCudaDevicesBrowserWidget::refresh () {

    emit refreshCudaDevices();
}

void SeerCudaDevicesBrowserWidget::showEvent (QShowEvent* event) {

    QWidget::showEvent(event);

    refresh();
}

