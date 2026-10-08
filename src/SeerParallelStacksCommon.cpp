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

int SeerParallelStacksFrame::depth () const {
    return _depth;
}

void SeerParallelStacksFrame::setDepth (int depth) {
    _depth = depth;
}

const QString& SeerParallelStacksFrame::addr () const {
    return _addr;
}

const QString& SeerParallelStacksFrame::function () const {
    return _function;
}

QString SeerParallelStacksFrame::functionOrAddr () const {

    if (_function == "??") {
        return _addr;
    }

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
    // XXX return QString("level: %1, address: '%2', function: '%3', file: '%4', fullname: '%5'") .arg(_level).arg(_addr).arg(_function).arg(_file).arg(_fullname);
    return QString("level: %1, address: '%2', file: '%3', fullname: '%4'") .arg(_level).arg(_addr).arg(_file).arg(_fullname);
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

SeerParallelStacksThread SeerParallelStacksThread::withFrames (const SeerParallelStacksFrames& frames) const {

    SeerParallelStacksThread thread = *this;
    thread._frames = frames;

    return thread;
}

int SeerParallelStacksThread::indexOfFunction (const QString& function) const {

    // Frames are innermost-first, so the first match is the innermost call.
    for (int i = 0; i < _frames.size(); ++i) {
        if (_frames[i].functionOrAddr() == function) {
            return i;
        }
    }

    return -1;
}

QString SeerParallelStacksThread::toString() const {

    QString result = QString("Thread %1: #Frames %2").arg(_id).arg(QString::number(_frames.size()));

    for (const auto& f : _frames) {
        result += "\n  " + f.toString();
    }

    result += "\n";

    return result;
}

static SeerParallelStacksNode buildImpl(const SeerParallelStacksThreads& threads, const SeerParallelStacksFrame& currentFrame, int depth) {

    SeerParallelStacksNode node;
    node.depth    = depth;
    node.function = currentFrame;
    node.threads  = threads;

    // Group threads by the function at position [-depth-1] (bottom-up).
    QMap<QString, SeerParallelStacksFrame>            functionFrames;
    QMap<QString, QVector<SeerParallelStacksThread>>  functionThreads;

    int level = -depth - 1;

    for (const SeerParallelStacksThread& t : threads) {

        int idx = t.frames().size() + level; // convert negative index

        if (idx < 0 || idx >= t.frames().size()) {
            continue;
        }

        const SeerParallelStacksFrame& frame = t.frame(idx);
        const QString&                 fn    = frame.function();

        if (functionFrames.contains(fn) == false) {
            functionFrames[fn] = frame;
        }

        functionThreads[fn].append(t);
    }

    for (auto it = functionThreads.begin(); it != functionThreads.end(); ++it) {

        // Tag the representative frame with the CHILD's own depth (depth+1)
        // rather than trusting whichever thread it happened to come from:
        // depth-from-bottom is the same for every thread sharing this
        // call-tree position (that's the invariant the grouping above
        // relies on), so it stays correct however "current" is later
        // redefined by an interactive thread/frame selection — unlike
        // level(), which is only meaningful relative to whichever thread
        // this particular frame was sampled from.
        SeerParallelStacksFrame repFrame = functionFrames[it.key()];
        repFrame.setDepth(depth + 1);

        SeerParallelStacksNode child = buildImpl(it.value(), repFrame, depth + 1);
        node.children.append(child);
    }

    return node;
}

SeerParallelStacksNode SeerParallelStacksBuildParallelStacks(const SeerParallelStacksThreads& threads) {

    return buildImpl(threads, SeerParallelStacksFrame(), 0);
}

// ---------------------------------------------------------------
// fillStack — flatten SeerParallelStacksNode tree into Stack tree for graphing
// ---------------------------------------------------------------
SeerParallelStacksStack SeerParallelStacksFillStack(const SeerParallelStacksNode& node, bool downward) {

    SeerParallelStacksStack stack;
    stack.threadCount = node.threads.size();

    // Collect thread IDs for this node
    for (const SeerParallelStacksThread& t : node.threads) {
        stack.threadIds.append(t.id());
    }

    if (node.children.size() == 1) {
        // Merge single child into this stack (chain of frames).
        // Keep the IDs from the leaf (most specific) node.
        // Normally children are deeper (more toward the top of the call
        // stack) than this node, so their frames go first — the resulting
        // list reads top of stack (innermost) to bottom of stack
        // (outermost). When drawn downward (Method View's callers), the
        // children are this node's callers instead, so they go last to
        // keep that same innermost-to-outermost reading order.
        auto child = SeerParallelStacksFillStack(node.children[0], downward);
        stack.frames      = child.frames;
        stack.stacks      = child.stacks;
        stack.threadCount = child.threadCount;
        stack.threadIds   = child.threadIds;

        if (node.function.function().isEmpty() == false) {
            if (downward) {
                stack.frames.prepend(node.function);
            }else{
                stack.frames.append(node.function);
            }
        }
    } else {
        if (node.function.function().isEmpty() == false) {
            stack.frames.append(node.function);
        }

        for (const auto& childNode : node.children) {
            stack.stacks.append(SeerParallelStacksFillStack(childNode, downward));
        }
    }

    return stack;
}

// Re-tags every frame in the tree with a signed distance from Method View's
// pivot, in place of buildImpl()'s depth-from-bottom: depth() becomes
// sign * (node.depth + offset).
static void renumberDepths(SeerParallelStacksNode& node, int offset, int sign) {

    if (node.function.function().isEmpty() == false) {
        node.function.setDepth(sign * (node.depth + offset));
    }

    for (auto& child : node.children) {
        renumberDepths(child, offset, sign);
    }
}

// ---------------------------------------------------------------
// Method View — split every thread that calls pivotFunction at its
// innermost call of it (so, with recursion, the callee half never contains
// the pivot again), then build each half with the same buildImpl() the
// Threads view uses:
//
//   callees: frames[0..idx]         — the pivot is the last (outermost)
//            frame, so the tree's root has exactly one child: the pivot.
//   callers: frames[idx+1..], reversed — so the immediate caller is the
//            outermost frame, and the tree grows from the pivot out
//            toward each thread's entry point.
// ---------------------------------------------------------------
SeerParallelStacksMethodStacks SeerParallelStacksBuildMethodStacks(const SeerParallelStacksThreads& threads, const QString& pivotFunction) {

    SeerParallelStacksMethodStacks result;
    result.pivotFunction = pivotFunction;

    SeerParallelStacksThreads calleeThreads;
    SeerParallelStacksThreads callerThreads;

    for (const SeerParallelStacksThread& t : threads) {

        int idx = t.indexOfFunction(pivotFunction);

        if (idx < 0) {
            continue;
        }

        const SeerParallelStacksFrames& frames = t.frames();

        SeerParallelStacksFrames callees = frames.mid(0, idx + 1);
        SeerParallelStacksFrames callers;

        for (int i = frames.size() - 1; i > idx; --i) {
            callers.append(frames[i]);
        }

        calleeThreads.append(t.withFrames(callees));
        callerThreads.append(t.withFrames(callers));
    }

    result.threadCount = calleeThreads.size();

    if (result.threadCount == 0) {
        return result;
    }

    // Callees. The pivot node is at buildImpl() depth 1, so offset -1 makes
    // it depth 0 and its callees +1, +2, ...
    SeerParallelStacksNode calleeRoot = buildImpl(calleeThreads, SeerParallelStacksFrame(), 0);
    renumberDepths(calleeRoot, -1, +1);

    if (calleeRoot.children.size() == 1) {

        // Build the pivot's box by hand rather than via FillStack(), which
        // would merge it into its callee chain whenever there's only one
        // callee branch — the pivot should always sit in a box of its own.
        const SeerParallelStacksNode& pivotNode = calleeRoot.children[0];

        result.callees.threadCount = pivotNode.threads.size();

        for (const SeerParallelStacksThread& t : pivotNode.threads) {
            result.callees.threadIds.append(t.id());
        }

        result.callees.frames.append(pivotNode.function);

        for (const auto& childNode : pivotNode.children) {
            result.callees.stacks.append(SeerParallelStacksFillStack(childNode));
        }
    }

    // Callers. The immediate caller is at buildImpl() depth 1, so negating
    // gives -1, -2, ... going outward.
    SeerParallelStacksNode callerRoot = buildImpl(callerThreads, SeerParallelStacksFrame(), 0);
    renumberDepths(callerRoot, 0, -1);

    result.callers = SeerParallelStacksFillStack(callerRoot, true);

    return result;
}

