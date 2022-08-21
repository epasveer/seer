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
        bool                    gdbHandleTerminatingException                   () const;
        bool                    gdbRandomizeStartAddress                        () const;
        bool                    gdbEnablePrettyPrinting                         () const;

        void                    setGdbProgram                                   (const QString& program);
        void                    setGdbArguments                                 (const QString& arguments);
        void                    setGdbAsyncMode                                 (bool flag);
        void                    setGdbHandleTerminatingException                (bool flag);
        void                    setGdbRandomizeStartAddress                     (bool flag);
        void                    setGdbEnablePrettyPrinting                      (bool flag);

        QString                 dprintfStyle                                    () const;
        QString                 dprintfFunction                                 () const;
        QString                 dprintfChannel                                  () const;

        void                    setDprintfStyle                                 (const QString& style);
        void                    setDprintfFunction                              (const QString& function);
        void                    setDprintfChannel                               (const QString& channel);

        void                    reset                                           ();

    protected slots:
        void                    handleGdbProgramToolButton                      ();
        void                    handleDprintfButtonGroup                        ();
};

