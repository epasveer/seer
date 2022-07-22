#pragma once

#include <QtWidgets/QWidget>

#include "ui_SeerAssemblyConfigPage.h"

class SeerAssemblyConfigPage : public QWidget, protected Ui::SeerAssemblyConfigPage {

    Q_OBJECT

    public:
        explicit SeerAssemblyConfigPage (QWidget* parent = 0);
       ~SeerAssemblyConfigPage ();

        bool                    showAssemblyTabOnStartup                        () const;
        bool                    keepAssemblyTabOnTop                            () const;
        QString                 disassembyFlavor                                () const;
        QString                 symbolDemagling                                 () const;
        QString                 registerFormat                                  () const;

        void                    setShowAssemblyTabOnStartup                     (bool flag) const;
        void                    setKeepAssemblyTabOnTop                         (bool flag) const;
        void                    setDisassembyFlavor                             (const QString& flavor) const;
        void                    setSymbolDemagling                              (const QString& onoff) const;
        void                    setRegisterFormat                               (const QString& format) const;

        void                    reset                                           ();

    protected slots:
};

