#pragma once

#include <QtWidgets/QWidget>

#include "ui_SeerSeerConfigPage.h"

class SeerSeerConfigPage : public QWidget, public Ui::SeerSeerConfigPage {

    Q_OBJECT

    public:
        explicit SeerSeerConfigPage (QWidget* parent = 0);
       ~SeerSeerConfigPage ();

        void                    setConsoleMode                                  (const QString& mode);
        QString                 consoleMode                                     () const;

        void                    setConsoleScrollLines                           (int count);
        int                     consoleScrollLines                              () const;

        void                    setRememberWindowSizes                          (bool flag);
        bool                    rememberWindowSizes                             () const;

        void                    setRememberManualCommandCount                   (int count);
        int                     rememberManualCommandCount                      () const;

        void                    setClearManualCommandHistory                    (bool flag);
        bool                    clearManualCommandHistory                       () const;
};

