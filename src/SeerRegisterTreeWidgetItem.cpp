#include "SeerRegisterTreeWidgetItem.h"
#include <QtWidgets/QTreeWidget>
#include <QtWidgets/QTreeWidgetItem>
#include <QtCore/QDebug>

//
// SeerRegisterTreeWidgetItem
//
bool SeerRegisterTreeWidgetItem::operator< (const QTreeWidgetItem& other) const {

    int column = treeWidget()->sortColumn();

    // If no sort column set, then default to ascending order.
    if (column < 0) {
        return true;
    }

    // If sort on column 0, then compare numerically.
    if (column == 0) {
        return text(column).toInt() < other.text(column).toInt();
    }

    // Otherwise, all other columns are textual.
    return text(column) < other.text(column);
}

