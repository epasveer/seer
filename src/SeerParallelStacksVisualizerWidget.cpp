// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "SeerParallelStacksVisualizerWidget.h"
#include "SeerHelpPageDialog.h"
#include "SeerUtl.h"
#include <QtWidgets/QMessageBox>
#include <QtWidgets/QFileDialog>
#include <QtGui/QIntValidator>
#include <QtGui/QIcon>
#include <QtPrintSupport/QPrinter>
#include <QtPrintSupport/QPrintDialog>
#include <QtCore/QSettings>
#include <QtCore/QDebug>

SeerParallelStacksVisualizerWidget::SeerParallelStacksVisualizerWidget (QWidget* parent) : QWidget(parent) {

    // Init variables.
    _id = Seer::createID(); // ID for parallelstacks command.

    // Set up UI.
    setupUi(this);

    // Setup the widgets
    setWindowIcon(QIcon(":/seer/resources/icons/hicolor/64x64/seergdb.png"));
    setWindowTitle("Seer ParallelStacks Visualizer");
    setAttribute(Qt::WA_DeleteOnClose);

    // Connect things.
    QObject::connect(refreshToolButton,  &QToolButton::clicked,      this,  &SeerParallelStacksVisualizerWidget::handleRefreshButton);
    QObject::connect(helpToolButton,     &QToolButton::clicked,      this,  &SeerParallelStacksVisualizerWidget::handleHelpButton);
    QObject::connect(printToolButton,    &QToolButton::clicked,      this,  &SeerParallelStacksVisualizerWidget::handlePrintButton);
    QObject::connect(saveToolButton,     &QToolButton::clicked,      this,  &SeerParallelStacksVisualizerWidget::handleSaveButton);

    // Restore window settings.
    readSettings();
}

SeerParallelStacksVisualizerWidget::~SeerParallelStacksVisualizerWidget () {
}

void SeerParallelStacksVisualizerWidget::handleText (const QString& text) {

    QApplication::setOverrideCursor(Qt::BusyCursor);

    qDebug() << text;

    QApplication::restoreOverrideCursor();
}

void SeerParallelStacksVisualizerWidget::handleRefreshButton () {

    // Clear the status.
    messageLineEdit->setText("");

    emit refreshParallelStackFrames(_id);
}

void SeerParallelStacksVisualizerWidget::handleHelpButton () {

    SeerHelpPageDialog* help = new SeerHelpPageDialog;
    help->loadFile(":/seer/resources/help/ImageVisualizer.md");
    help->show();
    help->raise();
}

void SeerParallelStacksVisualizerWidget::handlePrintButton () {

    imageViewer->print();
}

void SeerParallelStacksVisualizerWidget::handleSaveButton () {

    imageViewer->saveFileDialog("/tmp/temp.png");
}

void SeerParallelStacksVisualizerWidget::writeSettings() {

    QSettings settings;

    settings.beginGroup("parallelstacksvisualizerwindow");
    settings.setValue("size", size());
    settings.endGroup();
}

void SeerParallelStacksVisualizerWidget::readSettings() {

    QSettings settings;

    settings.beginGroup("parallelstacksvisualizerwindow");
    resize(settings.value("size", QSize(800, 400)).toSize());
    settings.endGroup();
}

void SeerParallelStacksVisualizerWidget::resizeEvent (QResizeEvent* event) {

    writeSettings();

    QWidget::resizeEvent(event);
}

