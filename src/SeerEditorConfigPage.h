#pragma once

#include "SeerHighlighterSettings.h"
#include <QtGui/QFont>
#include <QtWidgets/QWidget>

#include "ui_SeerEditorConfigPage.h"

class SeerEditorConfigPage : public QWidget, public Ui::SeerEditorConfigPage {

    Q_OBJECT

    public:
        explicit SeerEditorConfigPage (QWidget* parent = 0);
       ~SeerEditorConfigPage ();

        void                                setEditorFont                   (const QFont& font);
        const QFont&                        editorFont                      () const;

        void                                setEditorTabSize                (int spaces);
        int                                 editorTabSize                   () const;

        void                                setHighlighterSettings          (const SeerHighlighterSettings& settings);
        const SeerHighlighterSettings&      highlighterSettings             () const;

        void                                setHighlighterEnabled           (bool flag);
        bool                                highlighterEnabled              () const;

        void                                setExternalEditorCommand        (const QString& externalEditorCommand);
        QString                             externalEditorCommand           () const;

        void                                reset                           ();

    protected slots:
        void                                handleFontSizeChanged           (const QString& text);
        void                                handleFontChanged               (const QFont& font);
        void                                handleFontDialog                ();
        void                                handleHighlighterChanged        ();
        void                                handleEnabledChanged            ();
        void                                handleApplyTheme                ();

    private:
        QFont                               _font;
        SeerHighlighterSettings             _highlighterSettings;
};

