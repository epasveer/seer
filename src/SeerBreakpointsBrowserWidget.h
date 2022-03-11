#pragma once

#include <QtWidgets/QWidget>
#include <QtCore/QString>
#include <QtCore/QStringList>
#include "ui_SeerBreakpointsBrowserWidget.h"

class SeerBreakpointsBrowserWidget : public QWidget, protected Ui::SeerBreakpointsBrowserWidgetForm {

    Q_OBJECT

    public:
        explicit SeerBreakpointsBrowserWidget (QWidget* parent = 0);
       ~SeerBreakpointsBrowserWidget ();

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
        void                refreshBreakpointsList      ();
        void                deleteBreakpoints           (QString breakpoints);
        void                enableBreakpoints           (QString breakpoints);
        void                disableBreakpoints          (QString breakpoints);
        void                insertBreakpoint            (QString breakpoint);
        void                selectedFile                (QString file, QString fullname, int lineno);

    protected:
        void                showEvent                   (QShowEvent* event);

    private:
};

