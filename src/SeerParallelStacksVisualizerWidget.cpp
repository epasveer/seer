// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "SeerParallelStacksVisualizerWidget.h"
#include "SeerParallelStacksSettingsDialog.h"
#include "SeerParallelStacksCommon.h"
#include "SeerHelpPageDialog.h"
#include "SeerUtl.h"
#include <QtWidgets/QMessageBox>
#include <QtWidgets/QFileDialog>
#include <QtWidgets/QComboBox>
#include <QtGui/QIntValidator>
#include <QtGui/QIcon>
#if QT_VERSION >= QT_VERSION_CHECK(6, 6, 3)
#include <QtGui/QGuiApplication>
#include <QtGui/QStyleHints>
#endif
#include <QtPrintSupport/QPrinter>
#include <QtPrintSupport/QPrintDialog>
#include <QtCore/QSettings>
#include <QtCore/QProcess>
#include <QtCore/QStringList>
#include <QtCore/QFile>
#include <QtCore/QSignalBlocker>
#include <QtCore/QTimer>
#include <QtCore/QDebug>

SeerParallelStacksVisualizerWidget::SeerParallelStacksVisualizerWidget (QWidget* parent) : QWidget(parent) {

    // Init variables.
    _id       = Seer::createID(); // ID for parallelstacks command.
    _settings = {"Auto", "Stack", true, 64, true, 20};

    // Set up UI.
    setupUi(this);

    // Setup the widgets
    setWindowIcon(QIcon(":/seer/resources/icons/hicolor/64x64/seergdb.png"));
    setWindowTitle("Seer ParallelStacks Visualizer (BETA - please report bugs/features)");
    setAttribute(Qt::WA_DeleteOnClose);

    // Connect things.
    QObject::connect(refreshToolButton,             &QToolButton::clicked,                           this,  &SeerParallelStacksVisualizerWidget::handleRefreshButton);
    QObject::connect(helpToolButton,                &QToolButton::clicked,                           this,  &SeerParallelStacksVisualizerWidget::handleHelpButton);
    QObject::connect(printToolButton,               &QToolButton::clicked,                           this,  &SeerParallelStacksVisualizerWidget::handlePrintButton);
    QObject::connect(saveToolButton,                &QToolButton::clicked,                           this,  &SeerParallelStacksVisualizerWidget::handleSaveButton);
    QObject::connect(settingsToolButton,            &QToolButton::clicked,                           this,  &SeerParallelStacksVisualizerWidget::handleSettingsButton);
#if QT_VERSION >= QT_VERSION_CHECK(6, 6, 3)
    QObject::connect(QGuiApplication::styleHints(), &QStyleHints::colorSchemeChanged,                this,  &SeerParallelStacksVisualizerWidget::handleThemeChanged);
#endif
    QObject::connect(graphicsView,                  &SeerParallelStacksGraphicsView::selectedThread, this,  &SeerParallelStacksVisualizerWidget::handleGraphThreadSelected);
    QObject::connect(viewModeComboBox,              &QComboBox::currentTextChanged,                  this,  &SeerParallelStacksVisualizerWidget::handleViewModeChanged);
    QObject::connect(autoRefreshCheckBox,           &QCheckBox::toggled,                             this,  &SeerParallelStacksVisualizerWidget::writeSettings);

    // Colorize icons and the graph for theme.
    Seer::colorizeAllIcons(this, Seer::iconColorTheme());
    graphicsView->setColorTheme(Seer::iconColorTheme());

    // Restore window settings.
    readSettings();
}

SeerParallelStacksVisualizerWidget::~SeerParallelStacksVisualizerWidget () {

    if (QGraphicsScene* scene = graphicsView->scene()) {
        scene->clear();
    }
}

void SeerParallelStacksVisualizerWidget::setSettings (const SeerParallelStacksSettings& settings) {

    _settings = settings;

    writeSettings();
}

SeerParallelStacksSettings SeerParallelStacksVisualizerWidget::settings () const {

    return _settings;
}

void SeerParallelStacksVisualizerWidget::setShowFullFunctionName (bool flag) {

    _settings.showFullFunctionName = flag;

    writeSettings();

    // Redraw scene with new settings.
    createDirectedGraph();
    highlightDirectedGraph(_currentThreadId, _currentFrameLevel);
}

bool SeerParallelStacksVisualizerWidget::showFullFunctionName () const {

    return _settings.showFullFunctionName;
}

void SeerParallelStacksVisualizerWidget::setFunctionNameLength (int length) {

    _settings.functionNameLength = length;

    writeSettings();

    // Redraw scene with new settings.
    createDirectedGraph();
    highlightDirectedGraph(_currentThreadId, _currentFrameLevel);
}

int SeerParallelStacksVisualizerWidget::functionNameLength () const {

    return _settings.functionNameLength;
}

void SeerParallelStacksVisualizerWidget::setShowMinimapMode (const QString& mode) {

    _settings.showMinimapMode = mode;

    writeSettings();

    // Redraw scene with new settings.
    createDirectedGraph();
    highlightDirectedGraph(_currentThreadId, _currentFrameLevel);
}

const QString& SeerParallelStacksVisualizerWidget::showMinimapMode () const {

    return _settings.showMinimapMode;
}

void SeerParallelStacksVisualizerWidget::refresh () {

    handleRefreshButton();
}

void SeerParallelStacksVisualizerWidget::highlightDirectedGraph (int threadId, int frameLevel) {

    // frameLevel is relative to threadId's own numbering. Convert it to a
    // depth-from-bottom value using that thread's own frame count, since
    // that's what the graph's boxes compare against (see
    // SeerParallelStacksFrame::depth()) — a raw level is only meaningful
    // relative to the thread it came from. depth() starts at 1 for the
    // outermost frame, so the conversion is frameCount - level, not
    // frameCount - 1 - level.
    //
    // In Method View, depth is instead the signed distance from the pivot
    // (see SeerParallelStacksFrame::depth()): pivot index - level. If this
    // thread never calls the pivot, it isn't in the graph, so no frame is.
    int frameDepth = kNoFrameDepth;

    for (const auto& t : _threads) {
        if (t.id() == threadId) {
            if (isMethodView()) {
                int pivotIdx = t.indexOfFunction(_methodPivot);
                if (pivotIdx >= 0) {
                    frameDepth = pivotIdx - frameLevel;
                }
            }else{
                frameDepth = t.frameCount() - frameLevel;
            }
            break;
        }
    }

    graphicsView->setCurrentHighlight(threadId, frameDepth);
}

void SeerParallelStacksVisualizerWidget::highlightSelectedThread (int threadId) {

    // Switching threads selects that thread's innermost frame, so the old
    // frame level (from some other thread) no longer applies.
    _currentThreadId   = threadId;
    _currentFrameLevel = 0;

    updateForSelection();
}

void SeerParallelStacksVisualizerWidget::highlightSelectedFrame (int frameLevel) {

    _currentFrameLevel = frameLevel;

    updateForSelection();
}

void SeerParallelStacksVisualizerWidget::handleGraphThreadSelected (int threadId) {

    // The graph has already highlighted threadId itself. Track it here too,
    // so a later frame selection (or Method View rebuild) uses it rather
    // than a stale thread.
    _currentThreadId   = threadId;
    _currentFrameLevel = 0;

    emit selectedThread(threadId);

    // This is reached from inside a box's popup-table signal, and a rebuild
    // clears the scene — deleting that box. Defer it until the signal has
    // unwound.
    if (isMethodView()) {
        QTimer::singleShot(0, this, [this]() {
            updateForSelection();
        });
    }
}

void SeerParallelStacksVisualizerWidget::updateForSelection () {

    if (isMethodView() && currentPivotFunction() != _methodPivot) {
        createDirectedGraph();
    }

    highlightDirectedGraph(_currentThreadId, _currentFrameLevel);
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
            }

            QString current_thread_id_text   = Seer::parseFirst(text, "current_thread_id=",   '"', '"', false);
            QString current_frame_level_text = Seer::parseFirst(text, "current_frame_level=", '"', '"', false);

            _currentThreadId   = current_thread_id_text.toInt();
            _currentFrameLevel = current_frame_level_text.toInt();

            createDirectedGraph();
            highlightDirectedGraph(_currentThreadId, _currentFrameLevel);
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

void SeerParallelStacksVisualizerWidget::handleSettingsButton () {

    // Bring up the settings dialog.
    SeerParallelStacksSettingsDialog dialog(this);

    // Set dialog things.
    dialog.setSettings(_settings);

    // Execute the dialog.
    if (dialog.exec()) {

        // Set things. The dialog doesn't know about the view mode (that's
        // the toolbar's toggle), so carry it across.
        SeerParallelStacksSettings settings = dialog.settings();
        settings.viewMode = _settings.viewMode;

        setSettings(settings);

        // Redraw scene with new settings.
        createDirectedGraph();
        highlightDirectedGraph(_currentThreadId, _currentFrameLevel);
    }
}

void SeerParallelStacksVisualizerWidget::handleThemeChanged () {

    // Colorize icons for theme.
    Seer::colorizeAllIcons(this, Seer::iconColorTheme());

    // And the graph.
    graphicsView->setColorTheme(Seer::iconColorTheme());
}

void SeerParallelStacksVisualizerWidget::handleViewModeChanged (const QString& mode) {

    _settings.viewMode = mode;

    writeSettings();

    // Redraw scene in the new mode.
    createDirectedGraph();
    highlightDirectedGraph(_currentThreadId, _currentFrameLevel);
}

void SeerParallelStacksVisualizerWidget::writeSettings() {

    QSettings settings;

    settings.beginGroup("parallelstacksvisualizerwindow"); {

        settings.setValue("size", size());
        settings.setValue("functionnamelength",   _settings.functionNameLength);
        settings.setValue("showfullfunctionname", _settings.showFullFunctionName);
        settings.setValue("showfullstacksize",    _settings.showFullStackSize);
        settings.setValue("stacksize",            _settings.stackSize);
        settings.setValue("showminimapmode",      _settings.showMinimapMode);
        settings.setValue("viewmode",             _settings.viewMode);
        settings.setValue("autorefresh",          autoRefreshCheckBox->isChecked());
    } settings.endGroup();
}

void SeerParallelStacksVisualizerWidget::readSettings() {

    QSettings settings;

    settings.beginGroup("parallelstacksvisualizerwindow"); {

        resize(settings.value("size", QSize(800, 400)).toSize());

        _settings.showFullFunctionName = settings.value("showfullfunctionname", true).toBool();
        _settings.functionNameLength   = settings.value("functionnamelength", 64).toInt();
        _settings.showFullStackSize    = settings.value("showfullstacksize", true).toBool();
        _settings.stackSize            = settings.value("stacksize", 20).toInt();
        _settings.showMinimapMode      = settings.value("showminimapmode", "Auto").toString();
        _settings.viewMode             = settings.value("viewmode", "Stack").toString();

        // Unknown (or older "Threads") values fall back to Stack.
        if (viewModeComboBox->findText(_settings.viewMode) < 0) {
            _settings.viewMode = "Stack";
        }

        // No graph to redraw yet, so don't let the change trigger one.
        QSignalBlocker blocker(viewModeComboBox);
        viewModeComboBox->setCurrentText(_settings.viewMode);

        autoRefreshCheckBox->setChecked(settings.value("autorefresh", false).toBool());

    } settings.endGroup();
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

    // Method View: pivot on the current thread's current function.
    if (isMethodView()) {

        _methodPivot = currentPivotFunction();

        SeerParallelStacksMethodStacks method = SeerParallelStacksBuildMethodStacks(_threads, _methodPivot);

        graphicsView->setMethodStacks(method, settings());

        if (_methodPivot.isEmpty()) {
            messageLineEdit->setText("Method View: no current frame to pivot on.");
        }else{
            messageLineEdit->setText(QString("Method View: %1 (%2 of %3 threads)").arg(_methodPivot).arg(method.threadCount).arg(_threads.size()));
        }

        return;
    }

    _methodPivot.clear();
    messageLineEdit->setText("");

    // Build parallel-stacks tree. Purely structural — no highlighting here;
    // see highlightDirectedGraph().
    SeerParallelStacksNode  root  = SeerParallelStacksBuildParallelStacks(_threads);
    SeerParallelStacksStack stack = SeerParallelStacksFillStack(root);

    graphicsView->setStack(stack, settings());
}


bool SeerParallelStacksVisualizerWidget::isMethodView () const {

    return _settings.viewMode == "Method";
}

QString SeerParallelStacksVisualizerWidget::currentPivotFunction () const {

    for (const auto& t : _threads) {
        if (t.id() == _currentThreadId) {
            if (_currentFrameLevel >= 0 && _currentFrameLevel < t.frameCount()) {
                return t.frame(_currentFrameLevel).functionOrAddr();
            }
            break;
        }
    }

    return "";
}

