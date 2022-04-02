#pragma once

#include <QtWidgets/QWidget>
#include "ui_SeerEditorOptionsBarWidget.h"

class SeerEditorOptionsBarWidget : public QWidget, protected Ui::SeerEditorOptionsBarWidgetForm {

    Q_OBJECT

    public:
        explicit SeerEditorOptionsBarWidget (QWidget* parent = 0);
       ~SeerEditorOptionsBarWidget ();

        QToolButton*            fileOpenToolButton          ();
        QToolButton*            fileCloseToolButton         ();
        QToolButton*            textSearchToolButton        ();
};

