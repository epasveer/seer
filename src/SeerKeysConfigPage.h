#pragma once

#include "SeerKeySettings.h"
#include <QtWidgets/QWidget>

#include "ui_SeerKeysConfigPage.h"

class SeerKeysConfigPage : public QWidget, protected Ui::SeerKeysConfigPage {

    Q_OBJECT

    public:
        explicit SeerKeysConfigPage (QWidget* parent = 0);
       ~SeerKeysConfigPage ();

        void                        setKeySettings              (const SeerKeySettings& settings);
        const SeerKeySettings&      keySettings                 () const;

    protected slots:

    private:
        SeerKeySettings             _keySettings;
};

