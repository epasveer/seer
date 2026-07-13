// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "SeerParallelStacksVisualizerWidget.h"
#include "SeerParallelStacksCommon.h"
#include "SeerHelpPageDialog.h"
#include "SeerUtl.h"
#include <QtWidgets/QMessageBox>
#include <QtWidgets/QFileDialog>
#include <QtGui/QIntValidator>
#include <QtGui/QIcon>
#include <QtPrintSupport/QPrinter>
#include <QtPrintSupport/QPrintDialog>
#include <QtCore/QSettings>
#include <QtCore/QProcess>
#include <QtCore/QStringList>
#include <QtCore/QFile>
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

    if (QGraphicsScene* scene = graphicsView->scene()) {
        scene->clear();
    }
}

void SeerParallelStacksVisualizerWidget::refresh () {

    handleRefreshButton();
}

void SeerParallelStacksVisualizerWidget::handleText (const QString& text) {

    QApplication::setOverrideCursor(Qt::BusyCursor);

    if (text.contains(QRegularExpression("^([0-9]+)\\^done,frames="))) {

        // 4^done,frames=[
        //        {thread-id="10",target-id="(26131, 26146, 0)",name="hellothreads5",state="stopped",current="0",
        //            frames=[
        //                {level="0",addr="0x7ffff78a3c1e",func="__futex_abstimed_wait_common",arch="i386:x86-64",from="/lib64/libc.so.6",type="normal"},
        //                {level="1",addr="0x7ffff78a6548",func="pthread_cond_wait@@GLIBC_2.3.2",arch="i386:x86-64",from="/lib64/libc.so.6",type="normal"},
        //                {level="2",addr="0x404357",func="w2",arch="i386:x86-64",file="hellothreads5.cpp",fullname="/nas/erniep/Development/seer/tests/hellothreads5/hellothreads5.cpp",line="22",type="normal"},
        //              ]
        //        },
        //  ],
        //  current_thread_id="1",current_frame_level="0",total_threads="11",total_frames="128"

        QString id_text = text.section('^', 0,0);

        if (id_text.toInt() == _id) {

            _threads.clear();

            QString result_text = Seer::parseFirst(text, "frames=", '[', ']', false);

            QStringList thread_list = Seer::parse(result_text, "", '{', '}', false);

            // Loop through each thread.
            for (const auto& thread_text : thread_list) {

                SeerParallelStacksThread thread(thread_text);

                _threads.push_back(thread);

                //qDebug().noquote() << thread_text;
            }

            //for (const auto& thread : _threads ) {
            //    qDebug() << "Thread" << thread.id() << "has" << thread.frameCount() << "frames.";
            //}

            createDirectedGraph();
        }

    }else if (text.startsWith("^error,msg=\"No registers.\"")) {

        // Clear old scene.
        if (QGraphicsScene* scene = graphicsView->scene()) {
            scene->clear();
        }

    // At a stopping point, refresh.
    }else if (text.startsWith("*stopped,reason=\"")) {

        if (autoRefreshCheckBox->isChecked()) {
            handleRefreshButton();
        }

    }else{
        // Ignore anything else.
    }

    QApplication::restoreOverrideCursor();
}

void SeerParallelStacksVisualizerWidget::handleRefreshButton () {

    // Clear the status.
    messageLineEdit->setText("");

    emit refreshParallelStackFrames(_id);
}

void SeerParallelStacksVisualizerWidget::handleHelpButton () {

    SeerHelpPageDialog* help = new SeerHelpPageDialog;
    help->loadFile(":/seer/resources/help/ParallelStacksVisualizer.md");
    help->show();
    help->raise();
}

void SeerParallelStacksVisualizerWidget::handlePrintButton () {

    QGraphicsScene* scene = graphicsView->scene();
    if (scene == nullptr) {
        return;
    }

    QPrinter printer(QPrinter::HighResolution);
    printer.setPageOrientation(QPageLayout::Landscape);

    QPrintDialog dialog(&printer, this);
    dialog.setWindowTitle("Seer - Print ParallelStacks");
    if (dialog.exec() != QDialog::Accepted) {
        return;
    }

    QRectF sceneRect = scene->sceneRect();

    // Render the scene to a high-resolution image first.
    // Use a decent multiplier so text/shapes stay crisp when scaled up to page size.
    const qreal renderScale = 4.0; // tweak for quality vs memory/time
    QSize imageSize = (sceneRect.size() * renderScale).toSize();

    QImage image(imageSize, QImage::Format_ARGB32);
    image.fill(Qt::white);

    QPainter imagePainter(&image);
    imagePainter.setRenderHint(QPainter::Antialiasing);
    imagePainter.setRenderHint(QPainter::TextAntialiasing);
    imagePainter.setRenderHint(QPainter::SmoothPixmapTransform);

    scene->render(&imagePainter, QRectF(QPointF(0, 0), image.size()), sceneRect);
    imagePainter.end();

    // Now print the rasterized image, scaled to fit the page.
    QPainter printPainter(&printer);
    printPainter.setRenderHint(QPainter::SmoothPixmapTransform);

    QRectF pageRect = printer.pageRect(QPrinter::DevicePixel);

    QRectF target(QPointF(0, 0), image.size());
    target.setSize(target.size().scaled(pageRect.size(), Qt::KeepAspectRatio));
    target.moveCenter(pageRect.center());

    printPainter.drawImage(target, image, image.rect());
    printPainter.end();

    QMessageBox::information(this, "Done", "Scene sent to printer.");
}

void SeerParallelStacksVisualizerWidget::handleSaveButton () {

    QGraphicsScene* scene = graphicsView->scene();
    if (scene == nullptr) {
        return;
    }

    QString fileName = QFileDialog::getSaveFileName(this, "Seer - Save ParallelStacks As Image", "parallelstacks.png", "PNG Image (*.png)");

    if (fileName.isEmpty()) {
        return;
    }

    // Make sure the file has a .png extension, in case the user typed one without it.
    if (!fileName.endsWith(".png", Qt::CaseInsensitive)) {
        fileName += ".png";
    }

    QRectF sceneRect = scene->sceneRect();

    // Render at a higher resolution than the scene's native size for crisper output.
    const qreal renderScale = 4.0; // tweak for quality vs file size
    QSize imageSize = (sceneRect.size() * renderScale).toSize();

    QImage image(imageSize, QImage::Format_ARGB32);
    image.fill(Qt::white); // use Qt::transparent if you want a transparent background

    QPainter painter(&image);
    painter.setRenderHint(QPainter::Antialiasing);
    painter.setRenderHint(QPainter::TextAntialiasing);
    painter.setRenderHint(QPainter::SmoothPixmapTransform);

    scene->render(&painter, QRectF(QPointF(0, 0), image.size()), sceneRect);
    painter.end();

    if (!image.save(fileName, "PNG")) {
        QMessageBox::warning(this, "Save Failed", "Could not save the image to:\n" + fileName);
        return;
    }

    QMessageBox::information(this, "Done", "Scene saved to:\n" + fileName);
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

void SeerParallelStacksVisualizerWidget::createDirectedGraph() {

    // Clear old scene.
    if (QGraphicsScene* scene = graphicsView->scene()) {
        scene->clear();
    }

    // Build parallel-stacks tree
    SeerParallelStacksNode  root  = SeerParallelStacksBuildParallelStacks(_threads);
    SeerParallelStacksStack stack = SeerParallelStacksFillStack(root);

    graphicsView->setStack(stack);
}

