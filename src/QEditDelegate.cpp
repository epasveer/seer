#include "QEditDelegate.h"

//
// QAllowEditDelegate enables a custom editor for a QTreeView column.
//
QAllowEditDelegate::QAllowEditDelegate(QObject* parent) : QStyledItemDelegate(parent) {
}

void QAllowEditDelegate::setModelData (QWidget* editor, QAbstractItemModel* model, const QModelIndex& index) const {

    emit editingStarted(index);

    QStyledItemDelegate::setModelData(editor, model, index);

    emit editingFinished(index);
}

//
// QNoEditDelegate disables editing in a QTreeView for a column.
// It does this by returning a 'null' editor when QTreeView calls QStyledItemDelegate::createEditor() for the cell.
//
QNoEditDelegate::QNoEditDelegate(QObject* parent) : QStyledItemDelegate(parent) {
}

QWidget* QNoEditDelegate::createEditor(QWidget* parent, const QStyleOptionViewItem& option, const QModelIndex& index) const {

    Q_UNUSED(parent);
    Q_UNUSED(option);
    Q_UNUSED(index);

    return 0;
}

