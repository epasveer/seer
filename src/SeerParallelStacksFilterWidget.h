// SPDX-FileCopyrightText: 2026 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include "SeerParallelStacksCommon.h"
#include <QtWidgets/QWidget>
#include <QtWidgets/QTreeWidget>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QPushButton>
#include <QtCore/QSet>
#include <QtCore/QString>

// Popup for the ParallelStacks filter (like Visual Studio's). A checkable
// tree of library -> function -> thread, built from every frame of every
// thread. Checking an item keeps only the threads that pass through it.
//
// The selection is reported as three sets, so it can be reapplied by name
// after a refresh:
//
//   libraries()  fully checked libraries.
//   functions()  fully checked functions not covered by a checked library.
//   threadIds()  individually checked threads under a partially checked function.
class SeerParallelStacksFilterWidget : public QWidget {

    Q_OBJECT

    public:
        explicit SeerParallelStacksFilterWidget (QWidget* parent = 0);
       ~SeerParallelStacksFilterWidget ();

        void                        setThreads                  (const SeerParallelStacksThreads& threads, const QSet<QString>& libraries, const QSet<QString>& functions, const QSet<int>& threadIds);

        QSet<QString>               libraries                   () const;
        QSet<QString>               functions                   () const;
        QSet<int>                   threadIds                   () const;

        // A frame's library: the 'from' shared object, or "(program)" for
        // frames that have none (the executable itself).
        static QString              libraryOf                   (const SeerParallelStacksFrame& frame);

    signals:
        void                        filterChanged               ();

    protected slots:
        void                        handleItemChanged           (QTreeWidgetItem* item, int column);
        void                        handleSearchTextChanged     (const QString& text);
        void                        handleClearButton           ();

    private:
        void                        emitFilterChanged           ();

        QLineEdit*                  _searchLineEdit;
        QTreeWidget*                _treeWidget;
        QPushButton*                _clearButton;
        bool                        _changePending = false;
};

