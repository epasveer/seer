#pragma once

#include "SeerEditorManagerEntry.h"
#include <QtWidgets/QDialog>
#include <QtWidgets/QWidget>

#include "ui_SeerCloseSourceDialog.h"

class SeerCloseSourceDialog : public QDialog, public Ui::SeerCloseSourceDialogForm {

    Q_OBJECT

    public:
        explicit SeerCloseSourceDialog (QWidget* parent = 0);
       ~SeerCloseSourceDialog ();

        void                        setFiles                    (const SeerEditorManagerFiles& files);
        SeerEditorManagerFiles      files                       () const;
        SeerEditorManagerFiles      selectedFiles               () const;

    protected slots:

    protected:
        void                        writeSettings               ();
        void                        readSettings                ();
        void                        resizeEvent                 (QResizeEvent* event);

    private:
};

