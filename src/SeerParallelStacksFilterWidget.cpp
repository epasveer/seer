// SPDX-FileCopyrightText: 2026 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "SeerParallelStacksFilterWidget.h"
#include <QtWidgets/QVBoxLayout>
#include <QtWidgets/QHBoxLayout>
#include <QtWidgets/QHeaderView>
#include <QtCore/QFileInfo>
#include <QtCore/QMap>
#include <QtCore/QTimer>
#include <QtCore/QSignalBlocker>
#include <QtCore/QDebug>

// Tree item roles. NameRole holds the library path, function name or
// thread id; KindRole says which of the three the item is.
static const int NameRole = Qt::UserRole;
static const int KindRole = Qt::UserRole + 1;

enum FilterItemKind {
    LibraryKind  = 0,
    FunctionKind = 1,
    ThreadKind   = 2
};

SeerParallelStacksFilterWidget::SeerParallelStacksFilterWidget (QWidget* parent) : QWidget(parent, Qt::Popup) {

    _searchLineEdit = new QLineEdit(this);
    _searchLineEdit->setPlaceholderText("Filter list");
    _searchLineEdit->setClearButtonEnabled(true);
    _searchLineEdit->setToolTip("Narrow the list to libraries and functions containing this text (case-insensitive).");

    _treeWidget = new QTreeWidget(this);
    _treeWidget->setColumnCount(1);
    _treeWidget->setHeaderHidden(true);
    _treeWidget->setToolTip("Check libraries, functions or threads to show only the threads that pass through them.");

    _clearButton = new QPushButton("Clear Filters", this);
    _clearButton->setToolTip("Uncheck everything and show all threads.");

    QHBoxLayout* searchLayout = new QHBoxLayout;
    searchLayout->addWidget(_searchLineEdit);
    searchLayout->addWidget(_clearButton);

    QVBoxLayout* layout = new QVBoxLayout(this);
    layout->setContentsMargins(4, 4, 4, 4);
    layout->addLayout(searchLayout);
    layout->addWidget(_treeWidget);

    resize(400, 450);

    // Connect things.
    QObject::connect(_treeWidget,       &QTreeWidget::itemChanged,      this, &SeerParallelStacksFilterWidget::handleItemChanged);
    QObject::connect(_searchLineEdit,   &QLineEdit::textChanged,        this, &SeerParallelStacksFilterWidget::handleSearchTextChanged);
    QObject::connect(_clearButton,      &QPushButton::clicked,          this, &SeerParallelStacksFilterWidget::handleClearButton);
}

SeerParallelStacksFilterWidget::~SeerParallelStacksFilterWidget () {
}

QString SeerParallelStacksFilterWidget::libraryOf (const SeerParallelStacksFrame& frame) {

    if (frame.from().isEmpty()) {
        return "(program)";
    }

    return frame.from();
}

void SeerParallelStacksFilterWidget::setThreads (const SeerParallelStacksThreads& threads, const QSet<QString>& libraries, const QSet<QString>& functions, const QSet<int>& threadIds) {

    // Collect library -> function -> thread id -> thread name. QMap keeps
    // each level sorted, and collapses a recursive function's repeats.
    QMap<QString, QMap<QString, QMap<int, QString>>> tree;

    for (const auto& t : threads) {
        for (const auto& f : t.frames()) {
            tree[libraryOf(f)][f.functionOrAddr()][t.id()] = t.name();
        }
    }

    // Rebuilding sets every item's check state, which isn't a user change.
    QSignalBlocker blocker(_treeWidget);

    _treeWidget->clear();

    for (auto lib = tree.cbegin(); lib != tree.cend(); ++lib) {

        QTreeWidgetItem* libItem = new QTreeWidgetItem(_treeWidget);
        libItem->setText(0, lib.key() == "(program)" ? lib.key() : QFileInfo(lib.key()).fileName());
        libItem->setToolTip(0, lib.key());
        libItem->setData(0, NameRole, lib.key());
        libItem->setData(0, KindRole, LibraryKind);
        libItem->setFlags(libItem->flags() | Qt::ItemIsUserCheckable | Qt::ItemIsAutoTristate);

        for (auto func = lib.value().cbegin(); func != lib.value().cend(); ++func) {

            QTreeWidgetItem* funcItem = new QTreeWidgetItem(libItem);
            funcItem->setText(0, func.key());
            funcItem->setToolTip(0, QString("%1\n%2 thread%3").arg(func.key()).arg(func.value().size()).arg(func.value().size() == 1 ? "" : "s"));
            funcItem->setData(0, NameRole, func.key());
            funcItem->setData(0, KindRole, FunctionKind);
            funcItem->setFlags(funcItem->flags() | Qt::ItemIsUserCheckable | Qt::ItemIsAutoTristate);

            for (auto thr = func.value().cbegin(); thr != func.value().cend(); ++thr) {

                QTreeWidgetItem* threadItem = new QTreeWidgetItem(funcItem);
                threadItem->setText(0, thr.value().isEmpty() ? QString("Thread %1").arg(thr.key()) : QString("Thread %1 (%2)").arg(thr.key()).arg(thr.value()));
                threadItem->setData(0, NameRole, thr.key());
                threadItem->setData(0, KindRole, ThreadKind);
                threadItem->setFlags(threadItem->flags() | Qt::ItemIsUserCheckable);

                // Only the leaves are set. Auto-tristate parents derive their
                // own state from them.
                bool checked = libraries.contains(lib.key()) || functions.contains(func.key()) || threadIds.contains(thr.key());

                threadItem->setCheckState(0, checked ? Qt::Checked : Qt::Unchecked);
            }
        }

        libItem->setExpanded(true);
    }

    // Reapply the list's own narrowing to the new items.
    handleSearchTextChanged(_searchLineEdit->text());
}

QSet<QString> SeerParallelStacksFilterWidget::libraries () const {

    QSet<QString> set;

    for (int i = 0; i < _treeWidget->topLevelItemCount(); ++i) {

        QTreeWidgetItem* libItem = _treeWidget->topLevelItem(i);

        if (libItem->checkState(0) == Qt::Checked) {
            set.insert(libItem->data(0, NameRole).toString());
        }
    }

    return set;
}

QSet<QString> SeerParallelStacksFilterWidget::functions () const {

    QSet<QString> set;

    for (int i = 0; i < _treeWidget->topLevelItemCount(); ++i) {

        QTreeWidgetItem* libItem = _treeWidget->topLevelItem(i);

        // A fully checked library already covers its functions.
        if (libItem->checkState(0) != Qt::PartiallyChecked) {
            continue;
        }

        for (int j = 0; j < libItem->childCount(); ++j) {

            QTreeWidgetItem* funcItem = libItem->child(j);

            if (funcItem->checkState(0) == Qt::Checked) {
                set.insert(funcItem->data(0, NameRole).toString());
            }
        }
    }

    return set;
}

QSet<int> SeerParallelStacksFilterWidget::threadIds () const {

    QSet<int> set;

    for (int i = 0; i < _treeWidget->topLevelItemCount(); ++i) {

        QTreeWidgetItem* libItem = _treeWidget->topLevelItem(i);

        if (libItem->checkState(0) != Qt::PartiallyChecked) {
            continue;
        }

        for (int j = 0; j < libItem->childCount(); ++j) {

            QTreeWidgetItem* funcItem = libItem->child(j);

            // A fully checked function already covers its threads.
            if (funcItem->checkState(0) != Qt::PartiallyChecked) {
                continue;
            }

            for (int k = 0; k < funcItem->childCount(); ++k) {

                QTreeWidgetItem* threadItem = funcItem->child(k);

                if (threadItem->checkState(0) == Qt::Checked) {
                    set.insert(threadItem->data(0, NameRole).toInt());
                }
            }
        }
    }

    return set;
}

void SeerParallelStacksFilterWidget::handleItemChanged (QTreeWidgetItem* item, int column) {

    Q_UNUSED(item);
    Q_UNUSED(column);

    emitFilterChanged();
}

void SeerParallelStacksFilterWidget::handleSearchTextChanged (const QString& text) {

    // A library is shown if it, or any of its functions, matches. A
    // function is shown if it or its library matches. Threads follow their
    // function.
    for (int i = 0; i < _treeWidget->topLevelItemCount(); ++i) {

        QTreeWidgetItem* libItem  = _treeWidget->topLevelItem(i);
        bool             libMatch = text.isEmpty() || libItem->data(0, NameRole).toString().contains(text, Qt::CaseInsensitive);
        bool             anyFunc  = false;

        for (int j = 0; j < libItem->childCount(); ++j) {

            QTreeWidgetItem* funcItem  = libItem->child(j);
            bool             funcMatch = libMatch || funcItem->text(0).contains(text, Qt::CaseInsensitive);

            funcItem->setHidden(!funcMatch);

            anyFunc = anyFunc || funcMatch;
        }

        libItem->setHidden(!libMatch && !anyFunc);
    }
}

void SeerParallelStacksFilterWidget::handleClearButton () {

    // Unchecking the libraries cascades to everything under them.
    for (int i = 0; i < _treeWidget->topLevelItemCount(); ++i) {
        _treeWidget->topLevelItem(i)->setCheckState(0, Qt::Unchecked);
    }

    emitFilterChanged();
}

void SeerParallelStacksFilterWidget::emitFilterChanged () {

    // Checking a parent cascades an itemChanged for every item under it.
    // Coalesce them into one filterChanged once the cascade is done.
    if (_changePending) {
        return;
    }

    _changePending = true;

    QTimer::singleShot(0, this, [this]() {
        _changePending = false;
        emit filterChanged();
    });
}

