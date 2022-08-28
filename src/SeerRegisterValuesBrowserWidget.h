#pragma once

#include "ui_SeerRegisterValuesBrowserWidget.h"
#include <QtWidgets/QWidget>
#include <QtCore/QString>
#include <QtCore/QDebug>

class SeerRegisterValuesBrowserWidget : public QWidget, protected Ui::SeerRegisterValuesBrowserWidgetForm {

    Q_OBJECT

    public:
        explicit SeerRegisterValuesBrowserWidget (QWidget* parent = 0);
       ~SeerRegisterValuesBrowserWidget ();

    public:
        void                        setRegisterFormat           (QString fmt);

    public slots:
        void                        handleText                  (const QString& text);
        void                        handleStoppingPointReached  ();
        void                        refresh                     ();

    protected slots:
        void                        handleItemEntered           (QTreeWidgetItem* item, int column);
        void                        handleContextMenu           (const QPoint& pos);
        void                        handleIndexEditingFinished  (const QModelIndex& index);
        void                        handleFormatChanged         (int index);

    signals:
        void                        refreshRegisterNames        ();
        void                        refreshRegisterValues       (QString fmt);
        void                        setRegisterValue            (QString fmt, QString name, QString value);

    protected:
        void                        showEvent                   (QShowEvent* event);

    private:

};

