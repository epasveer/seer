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
        bool                    gdbNonStopMode                                  () const;
        bool                    gdbHandleTerminatingException                   () const;
        bool                    gdbRandomizeStartAddress                        () const;
        bool                    gdbEnablePrettyPrinting                         () const;
        QString                 gdbRemoteTargetType                             () const;


        void                    setGdbProgram                                   (const QString& program);
        void                    setGdbArguments                                 (const QString& arguments);
        void                    setGdbAsyncMode                                 (bool flag);
        void                    setGdbNonStopMode                               (bool flag);
        void                    setGdbHandleTerminatingException                (bool flag);
        void                    setGdbRandomizeStartAddress                     (bool flag);
        void                    setGdbEnablePrettyPrinting                      (bool flag);
        void                    setGdbRemoteTargetType                          (const QString& type);

        void                    reset                                           ();

    protected slots:
        void                    handleGdbProgramToolButton                      ();
};

