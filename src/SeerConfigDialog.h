#pragma once

#include "SeerGdbConfigPage.h"
#include "SeerEditorConfigPage.h"
#include "SeerSourceConfigPage.h"
#include "SeerSeerConfigPage.h"

#include <QtWidgets/QDialog>

#include "ui_SeerConfigDialog.h"

class QListWidget;
class QListWidgetItem;
class QStackedWidget;

class SeerConfigDialog : public QDialog, protected Ui::SeerConfigDialogForm {

    Q_OBJECT

    public:
        explicit SeerConfigDialog (QWidget* parent = 0);
       ~SeerConfigDialog ();

        // Gdb settings.
        void                    setGdbProgram                                   (const QString& program);
        QString                 gdbProgram                                      () const;

        void                    setGdbArguments                                 (const QString& arguments);
        QString                 gdbArguments                                    () const;

        void                    setGdbAsyncMode                                 (bool flag);
        bool                    gdbAsyncMode                                    () const;

        // Editor settings.
        void                    setEditorFont                                   (const QFont& font);
        const QFont&            editorFont                                      () const;

    public slots:
        void                    changePage              (QListWidgetItem* current, QListWidgetItem* previous);

    private:
        SeerGdbConfigPage*      _gdbConfigPage;
        SeerEditorConfigPage*   _editorConfigPage;
        SeerSourceConfigPage*   _sourceConfigPage;
        SeerSeerConfigPage*     _seerConfigPage;
};

