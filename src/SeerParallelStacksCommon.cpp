// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "SeerParallelStacksVisualizerWidget.h"
#include "SeerUtl.h"
#include <QtCore/QStringList>
#include <QtCore/QDebug>

SeerParallelStacksFrame::SeerParallelStacksFrame() {
}

SeerParallelStacksFrame::SeerParallelStacksFrame(const QString& text) {

    parse(text);
}

SeerParallelStacksFrame::~SeerParallelStacksFrame() {
}

void SeerParallelStacksFrame::parse (const QString& text) {

    _level     = Seer::parseFirst(text, "level=",    '"', '"', false).toInt();
    _addr      = Seer::parseFirst(text, "addr=",     '"', '"', false);
    _function  = Seer::parseFirst(text, "func=",     '"', '"', false);
    _arch      = Seer::parseFirst(text, "arch=",     '"', '"', false);
    _from      = Seer::parseFirst(text, "from=",     '"', '"', false);
    _file      = Seer::parseFirst(text, "file=",     '"', '"', false);
    _fullname  = Seer::parseFirst(text, "fullname=", '"', '"', false);
    _line      = Seer::parseFirst(text, "line=",     '"', '"', false).toInt();
    _type      = Seer::parseFirst(text, "type=",     '"', '"', false);
}

int SeerParallelStacksFrame::level () const {
    return _level;
}

const QString& SeerParallelStacksFrame::addr () const {
    return _addr;
}

const QString& SeerParallelStacksFrame::function () const {
    return _function;
}

const QString& SeerParallelStacksFrame::arch () const {
    return _arch;
}

const QString& SeerParallelStacksFrame::from () const {
    return _from;
}

const QString& SeerParallelStacksFrame::file () const {
    return _file;
}

const QString& SeerParallelStacksFrame::fullname () const {
    return _fullname;
}

int SeerParallelStacksFrame::line () const {
    return _line;
}

const QString& SeerParallelStacksFrame::type () const {
    return _type;
}

QString SeerParallelStacksFrame::toString() const {
    return QString("level: %1, address: '%2', function: '%3', file: '%4', fullname: '%5'") .arg(_level).arg(_addr).arg(_function).arg(_file).arg(_fullname);
}

SeerParallelStacksThread::SeerParallelStacksThread () {
}

SeerParallelStacksThread::SeerParallelStacksThread (const QString& text) {

    parse(text);
}

SeerParallelStacksThread::~SeerParallelStacksThread () {
}

void SeerParallelStacksThread::parse (const QString& text) {

    _id        = Seer::parseFirst(text, "thread-id=", '"', '"', false).toInt();
    _target_id = Seer::parseFirst(text, "target-id=", '"', '"', false);
    _name      = Seer::parseFirst(text, "name=",      '"', '"', false);
    _state     = Seer::parseFirst(text, "state=",     '"', '"', false);
    _current   = Seer::parseFirst(text, "current=",   '"', '"', false).toInt();

    QString frames_text = Seer::parseFirst(text, "frames=", '[', ']', false);

    QStringList frame_list  = Seer::parse(frames_text, "", '{', '}', false);

    // Loop through each thread.
    for (const auto& frame_text : frame_list) {
        SeerParallelStacksFrame frame(frame_text);
        _frames.push_back(frame);
    }
}

int SeerParallelStacksThread::id () const {
    return _id;
}

const QString& SeerParallelStacksThread::target_id () const {
    return _target_id;
}

const QString& SeerParallelStacksThread::name () const {
    return _name;
}

const QString& SeerParallelStacksThread::state () const {
    return _state;
}

int SeerParallelStacksThread::current () const {
    return _current;
}

int SeerParallelStacksThread::frameCount () const {
    return _frames.size();
}

const SeerParallelStacksFrame& SeerParallelStacksThread::frame (int i) const {
    return _frames[i];
}

const SeerParallelStacksFrames& SeerParallelStacksThread::frames () const {
    return _frames;
}

QString SeerParallelStacksThread::toString() const {

    QString result = QString("Thread %1").arg(_id);

    for (const auto& f : _frames) {
        result += "\n  " + f.toString();
    }

    return result;
}

static std::shared_ptr<SeerParallelStacksStackNode> buildImpl( QVector<SeerParallelStacksThread*>& threadPtrs, const QString& currentFunction, int depth) {

    auto node      = std::make_shared<SeerParallelStacksStackNode>();
    node->depth    = depth;
    node->function = currentFunction;
    node->threads  = threadPtrs;

    // Group threads by the function at position [-depth-1] (bottom-up).
    QMap<QString, QVector<SeerParallelStacksThread*>> functionThreads;
    int level = -depth - 1;

    for (SeerParallelStacksThread* t : threadPtrs) {
        int idx = t->frames().size() + level; // convert negative index
        if (idx < 0 || idx >= t->frames().size())
            continue;
        const QString& fn = t->frames()[idx].function();
        functionThreads[fn].append(t);
    }

    for (auto it = functionThreads.begin(); it != functionThreads.end(); ++it) {
        auto child = buildImpl(it.value(), it.key(), depth + 1);
        node->children.append(child);
    }

    return node;
}

std::shared_ptr<SeerParallelStacksStackNode> SeerParallelStacksBuildParallelStacks(QVector<SeerParallelStacksThread>& threads) {

    QVector<SeerParallelStacksThread*> ptrs;
    ptrs.reserve(threads.size());

    for (auto& t : threads) {
        ptrs.append(&t);
    }

    return buildImpl(ptrs, QString(), 0);
}

// ---------------------------------------------------------------
// fillStack — flatten SeerParallelStacksStackNode tree into Stack tree for graphing
// ---------------------------------------------------------------
std::shared_ptr<SeerParallelStacksStack> SeerParallelStacksFillStack(const std::shared_ptr<SeerParallelStacksStackNode>& node) {

    auto stack         = std::make_shared<SeerParallelStacksStack>();
    stack->threadCount = static_cast<int>(node->threads.size());

    // Collect thread IDs for this node
    for (const SeerParallelStacksThread* t : node->threads) {
        stack->threadIds.append(t->id());
    }

    if (!node->function.isEmpty()) {
        stack->functions.append(node->function);
    }

    if (node->children.size() == 1) {
        // Merge single child into this stack (chain of frames).
        // Keep the IDs from the leaf (most specific) node.
        auto child = SeerParallelStacksFillStack(node->children[0]);
        stack->functions  += child->functions;
        stack->stacks      = child->stacks;
        stack->threadCount = child->threadCount;
        stack->threadIds   = child->threadIds;
    } else {
        for (const auto& childNode : node->children) {
            stack->stacks.append(SeerParallelStacksFillStack(childNode));
        }
    }

    return stack;
}

