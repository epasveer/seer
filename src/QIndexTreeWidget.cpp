#include "QIndexTreeWidget.h"

QIndexTreeWidget::QIndexTreeWidget (QWidget* parent) : QTreeWidget(parent) {
}

QIndexTreeWidget::~QIndexTreeWidget () {
}

QTreeWidgetItem* QIndexTreeWidget::getItemFromIndex (const QModelIndex& index) const {

    return itemFromIndex(index);
}

