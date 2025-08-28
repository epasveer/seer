// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: MIT

#pragma once

#include <QtWidgets/QTreeWidget>
#include <QtWidgets/QTreeWidgetItem>
#include <QtWidgets/QItemDelegate>
#include <QtCore/QString>

class QIndexTreeWidget : public QTreeWidget {

    public:
        QIndexTreeWidget(QWidget* parent = nullptr);
       ~QIndexTreeWidget();

        QTreeWidgetItem*        getItemFromIndex            (const QModelIndex& index) const;
};

