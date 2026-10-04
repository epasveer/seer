// SPDX-FileCopyrightText: 2026 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include <QtWidgets/QWidget>
#include <QtCore/QVector>
#include <QtCore/QString>
#include <limits>

// Sentinel for "no frame depth" (no highlight, or a "[...]" placeholder
// row). Not -1, since Method View uses negative depths for callers.
constexpr int kNoFrameDepth = std::numeric_limits<int>::min();

class SeerParallelStacksFrame {
    public:
        SeerParallelStacksFrame ();
        SeerParallelStacksFrame (const QString& text);
       ~SeerParallelStacksFrame ();

        void                parse           (const QString& text);

        int                 level           () const;
        const QString&      addr            () const;
        const QString&      function        () const;
        QString             functionOrAddr  () const;
        const QString&      arch            () const;
        const QString&      from            () const;
        const QString&      file            () const;
        const QString&      fullname        () const;
        int                 line            () const;
        const QString&      type            () const;
        QString             toString        () const;

        // Distance from the bottom (outermost/root) frame of whichever
        // thread this frame was sampled from — unlike level() (that
        // thread's own absolute numbering, which varies with how many
        // frames that thread has below this point), depth() is the same
        // for every thread sharing this call-tree position, since it's set
        // from the tree's own recursion depth, not from any one thread's
        // frame count. Set by SeerParallelStacksCommon.cpp while building
        // the tree; kNoFrameDepth until then.
        //
        // In Method View the meaning changes to a signed distance from the
        // pivot frame instead: 0 for the pivot, +1, +2, ... for its callees
        // (upward) and -1, -2, ... for its callers (downward). Negative
        // values are therefore legitimate there — use kNoFrameDepth, not -1,
        // to mean "no frame".
        int                 depth           () const;
        void                setDepth        (int depth);

    private:
        int                 _level;
        QString             _addr;
        QString             _function;
        QString             _arch;
        QString             _from;
        QString             _file;
        QString             _fullname;
        int                 _line;
        QString             _type;
        int                 _depth = kNoFrameDepth;
};

typedef QVector<SeerParallelStacksFrame> SeerParallelStacksFrames;

class SeerParallelStacksThread {
    public:
        SeerParallelStacksThread ();
        SeerParallelStacksThread (const QString& text);
       ~SeerParallelStacksThread ();

        void                                    parse           (const QString& text);

        int                                     id              () const;
        const QString&                          target_id       () const;
        const QString&                          name            () const;
        const QString&                          state           () const;
        int                                     current         () const;
        QString                                 toString        () const;

        int                                     frameCount      () const;
        const SeerParallelStacksFrame&          frame           (int i) const;
        const SeerParallelStacksFrames&         frames          () const;

        // A copy of this thread (same id/name/state) with its frames
        // replaced — used by Method View to split a stack at the pivot.
        SeerParallelStacksThread                withFrames      (const SeerParallelStacksFrames& frames) const;

        // Index (== level) of the innermost frame whose function() is
        // function, or -1 if this thread never calls it.
        int                                     indexOfFunction (const QString& function) const;

    private:
        int                                     _id;
        QString                                 _target_id;
        QString                                 _name;
        QString                                 _state;
        int                                     _current;

        SeerParallelStacksFrames                _frames;
};

typedef QVector<SeerParallelStacksThread> SeerParallelStacksThreads;

struct SeerParallelStacksNode {
    SeerParallelStacksFrame                     function;    // function().isEmpty() == root
    int                                         depth             = 0;
    SeerParallelStacksThreads                   threads;
    QVector<SeerParallelStacksNode>             children;
};

// Flat "Stack" representation used when building the graph. Purely
// structural — which thread/frame is "current" is a separate, dynamic
// concern applied afterward (see SeerParallelStacksGraphicsView::
// setCurrentThreadId()/setCurrentFrameDepth()), not baked in here.
struct SeerParallelStacksStack {
    int                                         threadCount = 0;
    QVector<int>                                threadIds;   // IDs of every thread in this node
    SeerParallelStacksFrames                    frames;

    QVector<SeerParallelStacksStack>            stacks;
};

// Method View: the graph pivots on one function. The pivot sits alone in
// the callees root box; its callees branch upward from it and its callers
// branch downward, toward each thread's outermost frame.
struct SeerParallelStacksMethodStacks {
    QString                                     pivotFunction;
    int                                         threadCount = 0;    // threads whose stack contains the pivot
    SeerParallelStacksStack                     callees;            // root = the pivot box itself; children grow upward
    SeerParallelStacksStack                     callers;            // item-less root; children grow downward
};

struct SeerParallelStacksSettings {
    QString  showMinimapMode;
    QString  viewMode;              // "Threads" or "Method"
    bool     showFullFunctionName;
    int      functionNameLength;
    bool     showFullStackSize;
    int      stackSize;
};

SeerParallelStacksNode    SeerParallelStacksBuildParallelStacks     (const SeerParallelStacksThreads& threads);   // Build the parallel-stacks tree from a flat list of threads.
SeerParallelStacksStack   SeerParallelStacksFillStack               (const SeerParallelStacksNode& node, bool downward = false);  // downward: parent box sits above its children (Method View callers).
SeerParallelStacksMethodStacks SeerParallelStacksBuildMethodStacks  (const SeerParallelStacksThreads& threads, const QString& pivotFunction);  // Build Method View's two halves around pivotFunction.

