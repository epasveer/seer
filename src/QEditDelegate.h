#pragma once

#include <QtWidgets/QItemDelegate>
#include <QtWidgets/QStyledItemDelegate>
#include <QtCore/QModelIndex>

class QAllowEditDelegate : public QStyledItemDelegate {

    Q_OBJECT

    public:
        QAllowEditDelegate(QObject* parent = nullptr);

        virtual void        setModelData            (QWidget* editor, QAbstractItemModel* model, const QModelIndex& index) const;

    signals:
        void                editingFinished         (const QModelIndex& ) const;
};

class QNoEditDelegate : public QStyledItemDelegate {

    public:
        QNoEditDelegate(QObject* parent = nullptr);

        virtual QWidget* createEditor(QWidget* parent, const QStyleOptionViewItem& option, const QModelIndex& index) const;
};

