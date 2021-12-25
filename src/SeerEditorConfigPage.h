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
        void            handleSizeChanged               (const QString& text);
        void            handleFontChanged               (const QFont& font);
        void            handleFontDialog                ();

    private:
        QFont           _font;
};

