#pragma once

#include <QtWidgets/QWidget>
#include "ui_SeerRRConfigPage.h"

class SeerRRConfigPage : public QWidget, protected Ui::SeerRRConfigPage {

    Q_OBJECT

    public:
        explicit SeerRRConfigPage (QWidget* parent = 0);
       ~SeerRRConfigPage ();

        QString                 gdbProgram                                      () const;
        QString                 gdbArguments                                    () const;

        void                    setGdbProgram                                   (const QString& program);
        void                    setGdbArguments                                 (const QString& arguments);

        void                    reset                                           ();
};

