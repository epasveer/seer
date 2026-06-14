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

namespace Seer {
namespace PSV {

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

    QString Frame::toString() const {
        return QString("level: %1, address: '%2', function: '%3', file: '%4', fullname: '%5'")
            .arg(_level).arg(_addr).arg(_function).arg(_file).arg(_fullname);
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

    QString Thread::toString() const {

        QString result = QString("Thread %1").arg(_id);

        for (const auto &f : _frames)
            result += "\n  " + f.toString();

        return result;
    }

    static std::shared_ptr<StackNode> buildImpl( QVector<Thread *> &threadPtrs, const QString &currentFunction, int depth) {

        auto node      = std::make_shared<StackNode>();
        node->depth    = depth;
        node->function = currentFunction;
        node->threads  = threadPtrs;

        // Group threads by the function at position [-depth-1] (bottom-up).
        QMap<QString, QVector<Thread *>> functionThreads;
        int level = -depth - 1;

        for (Thread *t : threadPtrs) {
            int idx = t->frames().size() + level; // convert negative index
            if (idx < 0 || idx >= t->frames().size())
                continue;
            const QString &fn = t->frames()[idx].function();
            functionThreads[fn].append(t);
        }

        for (auto it = functionThreads.begin(); it != functionThreads.end(); ++it) {
            auto child = buildImpl(it.value(), it.key(), depth + 1);
            node->children.append(child);
        }

        return node;
    }

    std::shared_ptr<StackNode> buildParallelStacks(QVector<Thread> &threads) {

        QVector<Thread *> ptrs;
        ptrs.reserve(threads.size());

        for (auto &t : threads)
            ptrs.append(&t);

        return buildImpl(ptrs, QString(), 0);
    }

    // ---------------------------------------------------------------
    // fillStack — flatten StackNode tree into Stack tree for graphing
    // ---------------------------------------------------------------
    std::shared_ptr<Stack> fillStack(const std::shared_ptr<StackNode> &node) {

        auto stack         = std::make_shared<Stack>();
        stack->threadCount = static_cast<int>(node->threads.size());

        // Collect thread IDs for this node
        for (const Thread *t : node->threads)
            stack->threadIds.append(QString::number(t->id()));

        if (!node->function.isEmpty())
            stack->functions.append(node->function);

        if (node->children.size() == 1) {
            // Merge single child into this stack (chain of frames).
            // Keep the IDs from the leaf (most specific) node.
            auto child = fillStack(node->children[0]);
            stack->functions  += child->functions;
            stack->stacks      = child->stacks;
            stack->threadCount = child->threadCount;
            stack->threadIds   = child->threadIds;
        } else {
            for (const auto &childNode : node->children) {
                stack->stacks.append(fillStack(childNode));
            }
        }

        return stack;
    }

} // namespace PSV
} // namespace Seer
