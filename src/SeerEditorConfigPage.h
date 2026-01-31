// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

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

        void                                setAutoSourceReload             (bool flag);
        bool                                autoSourceReload                () const;

        void                                reset                           ();

    protected slots:
        void                                handleFontSizeChanged           (const QString& text);
        void                                handleFontChanged               (const QFont& font);
        void                                handleFontDialog                ();
        void                                handleHighlighterChanged        ();
        void                                handleEnabledChanged            ();
        void                                handleApplyTheme                ();
        void                                handleCppSuffixFocusIn          ();
        void                                handleRustSuffixFocusIn         ();
        void                                handleOdinSuffixFocusIn         ();

    private:
        QFont                               _font;
        SeerHighlighterSettings             _highlighterSettings;
};

