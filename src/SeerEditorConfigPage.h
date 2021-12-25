#pragma once

#include <QtGui/QFont>
#include <QtWidgets/QWidget>

#include "ui_SeerEditorConfigPage.h"

class SeerEditorConfigPage : public QWidget, public Ui::SeerEditorConfigPage {

    Q_OBJECT

    public:
        explicit SeerEditorConfigPage (QWidget* parent = 0);
       ~SeerEditorConfigPage ();

    protected slots:
        void            handlePointSizeChanged          (int i);
        void            handleFontChanged               (const QFont& font);
        void            handleFontDialog                ();

    private:
        void            _updateCodeTextEdit             ();
        QFont           _font;
};

