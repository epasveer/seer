// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include <QtWidgets/QTreeWidget>
#include <QtWidgets/QTreeWidgetItem>

class SeerRegisterTreeWidgetItem : public QTreeWidgetItem {

    public:
        /* Do we need to specify the base constructors?
        QTreeWidgetItem(const QTreeWidgetItem &other)
        QTreeWidgetItem(QTreeWidgetItem *parent, QTreeWidgetItem *preceding, int type = Type)
        QTreeWidgetItem(QTreeWidgetItem *parent, const QStringList &strings, int type = Type)
        QTreeWidgetItem(QTreeWidgetItem *parent, int type = Type)
        QTreeWidgetItem(QTreeWidget *parent, QTreeWidgetItem *preceding, int type = Type)
        QTreeWidgetItem(QTreeWidget *parent, const QStringList &strings, int type = Type)
        QTreeWidgetItem(QTreeWidget *parent, int type = Type)
        QTreeWidgetItem(const QStringList &strings, int type = Type)
        QTreeWidgetItem(int type = Type)
        */

        virtual bool operator< (const QTreeWidgetItem& other) const;
};

