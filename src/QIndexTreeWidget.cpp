// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: MIT

#include "QIndexTreeWidget.h"

QIndexTreeWidget::QIndexTreeWidget (QWidget* parent) : QTreeWidget(parent) {
}

QIndexTreeWidget::~QIndexTreeWidget () {
}

QTreeWidgetItem* QIndexTreeWidget::getItemFromIndex (const QModelIndex& index) const {

    return itemFromIndex(index);
}

