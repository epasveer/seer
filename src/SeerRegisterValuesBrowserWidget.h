#pragma once

#include "ui_SeerRegisterValuesBrowserWidget.h"
#include <QtWidgets/QWidget>
#include <QtWidgets/QItemDelegate>
#include <QtWidgets/QStyledItemDelegate>
#include <QtCore/QString>
#include <QtCore/QDebug>

class MyEditingDelegate : public QStyledItemDelegate {

    Q_OBJECT

    public:
        MyEditingDelegate(QObject* parent = nullptr) : QStyledItemDelegate(parent) {}

        virtual void setModelData (QWidget* editor, QAbstractItemModel* model, const QModelIndex& index) const {

            QStyledItemDelegate::setModelData(editor, model, index);

            emit editingFinished(index);
        }

    signals:
        void editingFinished(const QModelIndex& ) const;
};

class MyNoEditDelegate: public QStyledItemDelegate {
    public:
        MyNoEditDelegate(QObject* parent = nullptr): QStyledItemDelegate(parent) {}

        virtual QWidget* createEditor(QWidget* parent, const QStyleOptionViewItem& option, const QModelIndex& index) const {

            Q_UNUSED(parent);
            Q_UNUSED(option);
            Q_UNUSED(index);

            return 0;
        }
};

class SeerRegisterValuesBrowserWidget : public QWidget, protected Ui::SeerRegisterValuesBrowserWidgetForm {

    Q_OBJECT

    public:
        explicit SeerRegisterValuesBrowserWidget (QWidget* parent = 0);
       ~SeerRegisterValuesBrowserWidget ();

    public slots:
        void                handleText                  (const QString& text);
        void                handleStoppingPointReached  ();
        void                refresh                     ();

    protected slots:
        void                handleItemEntered           (QTreeWidgetItem* item, int column);
        void                handleIndexEditingFinished  (const QModelIndex& index);

    signals:
        void                refreshRegisterNames        ();
        void                refreshRegisterValues       ();
        void                setRegisterValue            (QString name, QString value);

    protected:
        void                showEvent                   (QShowEvent* event);

    private:

};

