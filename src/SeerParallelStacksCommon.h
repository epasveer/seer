// SPDX-FileCopyrightText: 2026 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include <QtWidgets/QWidget>
#include <QtCore/QVector>
#include <QtCore/QString>

class SeerParallelStacksFrame {
    public:
        SeerParallelStacksFrame ();
        SeerParallelStacksFrame (const QString& text);
       ~SeerParallelStacksFrame ();

        void                parse           (const QString& text);

        int                 level           () const;
        const QString&      addr            () const;
        const QString&      function        () const;
        const QString&      arch            () const;
        const QString&      from            () const;
        const QString&      file            () const;
        const QString&      fullname        () const;
        int                 line            () const;
        const QString&      type            () const;
        QString             toString        () const;

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
    QString                                     function;    // empty == root
    int                                         depth  = 0;
    QVector<SeerParallelStacksThread>           threads;
    QVector<SeerParallelStacksNode>             children;
};

// Flat "Stack" representation used when building the graph.
struct SeerParallelStacksStack {
    QVector<int>                                threadIds;   // IDs of every thread in this node
    QVector<QString>                            functions;

    QVector<SeerParallelStacksStack>            stacks;
    int                                         threadCount = 0;
};

SeerParallelStacksNode    SeerParallelStacksBuildParallelStacks     (const QVector<SeerParallelStacksThread>& threads);   // Build the parallel-stacks tree from a flat list of threads.
SeerParallelStacksStack   SeerParallelStacksFillStack               (const SeerParallelStacksNode& node);

