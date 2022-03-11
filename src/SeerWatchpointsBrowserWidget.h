#pragma once

#include <QtWidgets/QWidget>
#include <QtCore/QString>
#include "ui_SeerWatchpointsBrowserWidget.h"

class SeerWatchpointsBrowserWidget : public QWidget, protected Ui::SeerWatchpointsBrowserWidgetForm {

    Q_OBJECT

    public:
        explicit SeerWatchpointsBrowserWidget (QWidget* parent = 0);
       ~SeerWatchpointsBrowserWidget ();

        bool                isEmpty                     () const;

    public slots:
        void                handleText                  (const QString& text);
        void                handleStoppingPointReached  ();

    private slots:
        void                handleItemDoubleClicked     (QTreeWidgetItem* item, int column);
        void                handleRefreshToolButton     ();
        void                handleAddToolButton         ();
        void                handleDeleteToolButton      ();
        void                handleEnableToolButton      ();
        void                handleDisableToolButton     ();

    signals:
        void                refreshWatchpointsList      ();
        void                deleteWatchpoints           (QString watchpoints);
        void                enableWatchpoints           (QString watchpoints);
        void                disableWatchpoints          (QString watchpoints);
        void                insertWatchpoint            (QString watchpoint);
        void                selectedFile                (QString file, QString fullname, int lineno);

    protected:
        void                showEvent                   (QShowEvent* event);

    private:
};

