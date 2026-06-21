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

        int                 level           () const;
        const QString&      addr            () const;
        const QString&      function        () const;
        const QString&      arch            () const;
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

        int                 id              () const;
        const QString&      target_id       () const;
        const QString&      name            () const;
        const QString&      state           () const;
        int                 current         () const;
        QString             toString        () const;

        int                 frameCount      () const;
        const SeerParallelStacksFrame&        frame           (int i) const;
        const SeerParallelStacksFrames&       frames          () const;

    private:
        int                 _id;
        QString             _target_id;
        QString             _name;
        QString             _state;
        int                 _current;

        SeerParallelStacksFrames              _frames;
};

typedef QVector<SeerParallelStacksThread> SeerParallelStacksThreads;

struct SeerParallelStacksStackNode {
    QString                                 function;    // empty == root
    int                                     depth  = 0;
    QVector<SeerParallelStacksThread*>                        threads;     // non-owning pointers
    QVector<std::shared_ptr<SeerParallelStacksStackNode>>     children;
};

// Flat "Stack" representation used when building the graph.
struct SeerParallelStacksStack {
    QVector<QString>                        functions;
    QVector<std::shared_ptr<SeerParallelStacksStack>>         stacks;
    int                                     threadCount = 0;
    QVector<QString>                        threadIds;   // IDs of every thread in this node
};

std::shared_ptr<SeerParallelStacksStackNode>      SeerParallelStacksBuildParallelStacks     (QVector<SeerParallelStacksThread>& threads);   // Build the parallel-stacks tree from a flat list of threads.
std::shared_ptr<SeerParallelStacksStack>          SeerParallelStacksFillStack               (const std::shared_ptr<SeerParallelStacksStackNode>& node);

