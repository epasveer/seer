#pragma once

#include <QtWidgets/QWidget>
#include <QtCore/QString>
#include <QtCore/QStringList>
#include "ui_SeerPrintpointsBrowserWidget.h"

class SeerPrintpointsBrowserWidget : public QWidget, protected Ui::SeerPrintpointsBrowserWidgetForm {

    Q_OBJECT

    public:
        explicit SeerPrintpointsBrowserWidget (QWidget* parent = 0);
       ~SeerPrintpointsBrowserWidget ();

        bool                isEmpty                     () const;
        QStringList         breakpoints                 () const;

    public slots:
        void                handleText                  (const QString& text);
        void                handleStoppingPointReached  ();
        void                handleSessionTerminated     ();

    private slots:
        void                handleItemDoubleClicked     (QTreeWidgetItem* item, int column);
        void                handleRefreshToolButton     ();
        void                handleAddToolButton         ();
        void                handleDeleteToolButton      ();
        void                handleEnableToolButton      ();
        void                handleDisableToolButton     ();
        void                handleConditionToolButton   ();
        void                handleIgnoreToolButton      ();
        void                handleCommandToolButton     ();

    signals:
        void                refreshPrintpointsList      ();
        void                deletePrintpoints           (QString printpoints);
        void                enablePrintpoints           (QString printpoints);
        void                disablePrintpoints          (QString printpoints);
        void                addBreakpointCondition      (QString printpoint, QString condition);
        void                addBreakpointIgnore         (QString printpoint, QString count);
        void                addBreakpointCommand        (QString breakpoint, QString command);
        void                insertPrintpoint            (QString type, QString function, QString channel, QString parameters);
        void                selectedFile                (QString file, QString fullname, int lineno);

    protected:
        void                showEvent                   (QShowEvent* event);

    private:
};

