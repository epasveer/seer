#pragma once

#include <QtWidgets/QWidget>

#include "ui_SeerGdbConfigPage.h"

class SeerGdbConfigPage : public QWidget, protected Ui::SeerGdbConfigPage {

    Q_OBJECT

    public:
        explicit SeerGdbConfigPage (QWidget* parent = 0);
       ~SeerGdbConfigPage ();

        QString                 gdbProgram                                      () const;
        QString                 gdbArguments                                    () const;
        bool                    gdbAsyncMode                                    () const;

        void                    setGdbProgram                                   (const QString& program);
        void                    setGdbArguments                                 (const QString& arguments);
        void                    setGdbAsyncMode                                 (bool flag);

    protected slots:
        void                    handleGdbProgramToolButton                      ();
};

