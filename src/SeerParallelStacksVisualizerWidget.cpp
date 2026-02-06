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
#include <QtCore/QProcess>
#include <QtCore/QStringList>
#include <QtCore/QFile>
#include <QtCore/QDebug>

namespace Seer::PSV {

    Frame::Frame() {
    }

    Frame::Frame(const QString& text) {

        _level     = Seer::parseFirst(text, "level=",    '"', '"', false).toInt();
        _addr      = Seer::parseFirst(text, "addr=",     '"', '"', false);
        _function  = Seer::parseFirst(text, "func=",     '"', '"', false);
        _arch      = Seer::parseFirst(text, "arch=",     '"', '"', false);
        _file      = Seer::parseFirst(text, "file=",     '"', '"', false);
        _fullname  = Seer::parseFirst(text, "fullname=", '"', '"', false);
        _line      = Seer::parseFirst(text, "line=",     '"', '"', false).toInt();
        _type      = Seer::parseFirst(text, "type=",     '"', '"', false);
    }

    Frame::~Frame() {
    }

    int Frame::level () const {
        return _level;
    }

    const QString& Frame::addr () const {
        return _addr;
    }

    const QString& Frame::function () const {
        return _function;
    }

    const QString& Frame::arch () const {
        return _arch;
    }

    const QString& Frame::file () const {
        return _file;
    }

    const QString& Frame::fullname () const {
        return _fullname;
    }

    int Frame::line () const {
        return _line;
    }

    const QString& Frame::type () const {
        return _type;
    }

    Thread::Thread () {
    }

    Thread::Thread (const QString& text) {

        _id        = Seer::parseFirst(text, "thread-id=", '"', '"', false).toInt();
        _target_id = Seer::parseFirst(text, "target-id=", '"', '"', false);
        _name      = Seer::parseFirst(text, "name=",      '"', '"', false);
        _current   = Seer::parseFirst(text, "current=",   '"', '"', false).toInt();

        QString frames_text = Seer::parseFirst(text, "frames=", '[', ']', false);

        QStringList frame_list  = Seer::parse(frames_text, "", '{', '}', false);

        // Loop through each thread.
        for (const auto& frame_text : frame_list) {
            Frame frame(frame_text);
            _frames.push_back(frame);
        }
    }

    Thread::~Thread () {
    }

    int Thread::id () const {
        return _id;
    }

    const QString& Thread::target_id () const {
        return _target_id;
    }

    const QString& Thread::name () const {
        return _name;
    }

    const QString& Thread::state () const {
        return _state;
    }

    int Thread::current () const {
        return _current;
    }

    int Thread::frameCount () const {
        return _frames.size();
    }

    const Frame& Thread::frame (int i) const {
        return _frames[i];
    }

    const Frames& Thread::frames () const {
        return _frames;
    }

    GraphNode::GraphNode (const QString& name, QObject* parent) : QObject(parent) {

        _name = name;

        qDebug() << "Creating node:" << _name;
    }

    GraphNode::~GraphNode () {

         qDebug() << "Destroying node:" << _name;
    }

    QString GraphNode::name () const {

        return _name;
    }

    void GraphNode::addChild (GraphNode* child) {

        child->setParent(this);  // QObject handles parent-child relationship
    }

    GraphNode* GraphNode::getChild (int index) const {

        const QObjectList &childList = children();

        if (index >= 0 && index < childList.size()) {
            return qobject_cast<GraphNode*>(childList.at(index));
        }

        return nullptr;
    }

    int GraphNode::childCount () const {

        return children().size();
    }

    void GraphNode::addFrame (const Frame& frame) {

        _frames.push_back(frame);
    }

    const Frames& GraphNode::frames () const {

        return _frames;
    }

    int GraphNode::frameCount () const {

        return _frames.size();
    }

    void GraphNode::addThreadId (int id) {

        _threadIds.push_back(id);
    }

    const ThreadIds& GraphNode::threadIds () const {

        return _threadIds;
    }

    int GraphNode::threadIdCount () const {

        return _threadIds.size();
    }

    void GraphNode::printTree (int level) const {

        {
            QDebug dbg = qDebug().noquote().nospace();

            QString indent(level * 2, ' ');
            dbg << indent << "└─" << _name;
            dbg << " ThreadCount: " << threadIdCount();
            dbg << " Ids: ";
            for (int id : threadIds()) {
                dbg << id << ", ";
            }
            for (auto frame : frames()) {
                dbg << indent << "  " << frame.function() << '\n';
            }
        }

        for (QObject* child : children()) {
            GraphNode* node = qobject_cast<GraphNode*>(child);
            if (node) {
                node->printTree(level + 1);
            }
        }
    }
};


SeerParallelStacksVisualizerWidget::SeerParallelStacksVisualizerWidget (QWidget* parent) : QWidget(parent) {

    // Init variables.
    _id     = Seer::createID(); // ID for parallelstacks command.
    _gnodes = 0;

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

                Seer::PSV::Thread thread(thread_text);

                _threads.push_back(thread);

                //qDebug().noquote() << thread_text;
                //qDebug() << thread_id_text << target_id_text << name_text << current_text;
            }

            for (const auto& thread : _threads ) {
                qDebug() << "Thread" << thread.id() << "has" << thread.frameCount() << "frames.";
            }

            createDirectedGraph();
        }

    }else if (text.startsWith("^error,msg=\"No registers.\"")) {

        imageViewer->clearImage();

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

void SeerParallelStacksVisualizerWidget::createDirectedGraph() {

    // Clear old image.
    imageViewer->clearImage();

    // Create the 'gv' file.
    QFile file("/tmp/xxx.gv");

    if (file.open(QFile::WriteOnly|QFile::Text|QFile::Truncate) == false) {
        qDebug() << "Can't create 'gv' file!";
    }

    QTextStream out(&file);

    // Write 'gv' header.
    out << "digraph {\n";

    out << "\tgraph [rankdir=BT]\n";
    out << "\tnode [shape=plaintext]\n";

    // Loop through each stack and create the graph.
    for (int t=0; t<_threads.size(); t++) {

        out << "\t" << QString::number(t) << " [label=<<table BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\"><tr><td align=\"right\"><b>1 Threads</b></td></tr>";

        for (int f=0; f<_threads[t].frameCount(); f++) {

            const Seer::PSV::Frame& frame = _threads[t].frame(f);

            out << "<tr><td align=\"left\"><font color=\"darkgreen\">" << frame.function().toHtmlEscaped() << "</font></td></tr>";
        }

        out << "</table>>]\n";
    }

    // Write 'gv' footer.
    out << "}\n";

    // Close 'gv' file.
    file.close();

    // Now create the pdf file.
    // dot -Tpdf xxx.gv -o xxx.gv.pdf
    QString     program = "dot";
    QStringList arguments;

    arguments << "-Tsvg";
    arguments << "/tmp/xxx.gv";
    arguments << "-o";
    arguments << "/tmp/xxx.gv.svg";

    int exitCode = QProcess::execute(program, arguments);
    if (exitCode != 0) {
        qDebug() << "Command failed with exit code:" << exitCode;
        return;
    }

    // View the image.
    imageViewer->loadFile("/tmp/xxx.gv.svg");

    // Delete tmp files.
    QFile::remove("/tmp/xxx.gv");
    QFile::remove("/tmp/xxx.gv.svg");

    //
    // Make GraphNodes
    //

    if (_gnodes != 0) {
        delete _gnodes;
        _gnodes = 0;
    }

    _gnodes = new Seer::PSV::GraphNode("root");

    // Loop through each stack and create the graph.
    for (int t=0; t<_threads.size(); t++) {

        Seer::PSV::GraphNode* gnode = new Seer::PSV::GraphNode(QString::number(t));

        gnode->addThreadId(_threads[t].id());

        for (int f=0; f<_threads[t].frameCount(); f++) {

            const Seer::PSV::Frame& frame = _threads[t].frame(f);

            gnode->addFrame(frame);
        }

        _gnodes->addChild(gnode);
    }

    _gnodes->printTree();
}

