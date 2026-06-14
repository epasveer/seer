// SPDX-FileCopyrightText: 2026 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include <QtWidgets/QWidget>
#include <QtCore/QVector>
#include <QWidget>
#include <QGraphicsItem>
#include <QPointF>
#include <QString>

namespace Seer {
namespace PSV {

    class Frame {
        public:
            Frame ();
            Frame (const QString& text);
           ~Frame ();

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

    typedef QVector<Frame> Frames;

    class Thread {
        public:
            Thread ();
            Thread (const QString& text);
           ~Thread ();

            int                 id              () const;
            const QString&      target_id       () const;
            const QString&      name            () const;
            const QString&      state           () const;
            int                 current         () const;
            QString             toString        () const;

            int                 frameCount      () const;
            const Frame&        frame           (int i) const;
            const Frames&       frames          () const;

        private:
            int                 _id;
            QString             _target_id;
            QString             _name;
            QString             _state;
            int                 _current;

            Frames              _frames;
    };

    typedef QVector<Thread> Threads;

    typedef QVector<int>    ThreadIds;

    struct StackNode {
        QString               function;    // empty == root
        int                   depth  = 0;
        QVector<Thread *>     threads;     // non-owning pointers
        QVector<std::shared_ptr<StackNode>> children;
    };

    // Build the parallel-stacks tree from a flat list of threads.
    std::shared_ptr<StackNode> buildParallelStacks(QVector<Thread> &threads);

    // Flat "Stack" representation used when building the graph.
    struct Stack {
        QVector<QString>              functions;
        QVector<std::shared_ptr<Stack>> stacks;
        int                           threadCount = 0;
        QVector<QString>              threadIds;   // IDs of every thread in this node
    };

    std::shared_ptr<Stack> fillStack(const std::shared_ptr<StackNode> &node);

} // namespace PSV
} // namespace Seer

