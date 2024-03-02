#pragma once

#include "SeerGdbConfigPage.h"
#include "SeerRRConfigPage.h"
#include "SeerEditorConfigPage.h"
#include "SeerSourceConfigPage.h"
#include "SeerAssemblyConfigPage.h"
#include "SeerKeysConfigPage.h"
#include "SeerSeerConfigPage.h"
#include <QtWidgets/QDialog>

#include "ui_SeerConfigDialog.h"

class QListWidget;
class QListWidgetItem;
class QStackedWidget;

class SeerConfigDialog : public QDialog, protected Ui::SeerConfigDialogForm {

    Q_OBJECT

    public:
        explicit SeerConfigDialog (QWidget* parent = 0);
       ~SeerConfigDialog ();

        // Seer settings.
        void                                setSeerConsoleMode                              (const QString& mode);
        QString                             seerConsoleMode                                 () const;

        void                                setSeerConsoleScrollLines                       (int count);
        int                                 seerConsoleScrollLines                          () const;

        void                                setSeerRememberWindowSizes                      (bool flag);
        bool                                seerRememberWindowSizes                         () const;

        void                                setSeerRememberManualCommandCount               (int count);
        int                                 seerRememberManualCommandCount                  () const;

        void                                setSeerClearManualCommandHistory                (bool flag);
        bool                                seerClearManualCommandHistory                   () const;

        // Gdb settings.
        void                                setGdbProgram                                   (const QString& program);
        QString                             gdbProgram                                      () const;

        void                                setGdbArguments                                 (const QString& arguments);
        QString                             gdbArguments                                    () const;

        void                                setGdbAsyncMode                                 (bool flag);
        bool                                gdbAsyncMode                                    () const;

        void                                setGdbNonStopMode                               (bool flag);
        bool                                gdbNonStopMode                                  () const;

        void                                setGdbHandleTerminatingException                (bool flag);
        bool                                gdbHandleTerminatingException                   () const;

        void                                setGdbRandomizeStartAddress                     (bool flag);
        bool                                gdbRandomizeStartAddress                        () const;

        void                                setGdbEnablePrettyPrinting                      (bool flag);
        bool                                gdbEnablePrettyPrinting                         () const;

        void                                setDprintfStyle                                 (const QString& style);
        QString                             dprintfStyle                                    () const;

        void                                setDprintfFunction                              (const QString& function);
        QString                             dprintfFunction                                 () const;

        void                                setDprintfChannel                               (const QString& channel);
        QString                             dprintfChannel                                  () const;

        // Editor settings.
        void                                setEditorFont                                   (const QFont& font);
        const QFont&                        editorFont                                      () const;

        void                                setEditorTabSize                                (int spaces);
        int                                 editorTabSize                                   () const;

        void                                setEditorHighlighterSettings                    (const SeerHighlighterSettings& settings);
        const SeerHighlighterSettings&      editorHighlighterSettings                       () const;

        void                                setEditorHighlighterEnabled                     (bool flag);
        bool                                editorHighlighterEnabled                        () const;

        void                                setExternalEditorCommand                        (const QString& externalEditorCommand);
        QString                             externalEditorCommand                           () const;

        // Source settings.
        void                                setSourceAlternateDirectories                   (const QStringList& alternateDirectories);
        QStringList                         sourceAlternateDirectories                      () const;

        void                                setSourceIgnoreFilePatterns                     (const QStringList& filePatterns);
        QStringList                         sourceIgnoreFilePatterns                        () const;

        void                                setSourceMiscFilePatterns                       (const QStringList& filePatterns);
        QStringList                         sourceMiscFilePatterns                          () const;

        void                                setSourceSourceFilePatterns                     (const QStringList& filePatterns);
        QStringList                         sourceSourceFilePatterns                        () const;

        void                                setSourceHeaderFilePatterns                     (const QStringList& filePatterns);
        QStringList                         sourceHeaderFilePatterns                        () const;

        // Assembly settings.
        void                                setAssemblyShowAssemblyTabOnStartup             (bool flag);
        bool                                assemblyShowAssemblyTabOnStartup                () const;

        void                                setAssemblyKeepAssemblyTabOnTop                 (bool flag);
        bool                                assemblyKeepAssemblyTabOnTop                    () const;

        void                                setAssemblyDisassemblyFlavor                    (const QString& flavor);
        QString                             assemblyDisassemblyFlavor                       () const;

        void                                setAssemblySymbolDemagling                      (const QString& onoff);
        QString                             assemblySymbolDemagling                         () const;

        void                                setAssemblyShowAddressColumn                    (bool flag);
        bool                                assemblyShowAddressColumn                       () const;

        void                                setAssemblyShowOffsetColumn                     (bool flag);
        bool                                assemblyShowOffsetColumn                        () const;

        void                                setAssemblyShowOpcodeColumn                     (bool flag);
        bool                                assemblyShowOpcodeColumn                        () const;

        void                                setAssemblyShowSourceLines                      (bool flag);
        bool                                assemblyShowSourceLines                         () const;

        void                                setAssemblyRegisterFormat                       (const QString& format);
        QString                             assemblyRegisterFormat                          () const;

        void                                setAssemblyDisassemblyMode                      (const QString& mode, int bytes);
        QString                             assemblyDisassemblyMode                         () const;
        int                                 assemblyDisassemblyBytes                        () const;

        // Key settings.
        void                                setKeySettings                                  (const SeerKeySettings& settings);
        SeerKeySettings                     keySettings                                     () const;

        // RR settings.
        void                                setRRProgram                                    (const QString& program);
        QString                             rrProgram                                       () const;

        void                                setRRArguments                                  (const QString& arguments);
        QString                             rrArguments                                     () const;

        void                                setRRGdbArguments                               (const QString& arguments);
        QString                             rrGdbArguments                                  () const;

    public slots:
        void                                handleChangePage                                (QListWidgetItem* current, QListWidgetItem* previous);
        void                                handleButtonClicked                             (QAbstractButton* button);

    private:
        SeerGdbConfigPage*                  _gdbConfigPage;
        SeerRRConfigPage*                   _rrConfigPage;
        SeerEditorConfigPage*               _editorConfigPage;
        SeerSourceConfigPage*               _sourceConfigPage;
        SeerAssemblyConfigPage*             _assemblyConfigPage;
        SeerKeysConfigPage*                 _keysConfigPage;
        SeerSeerConfigPage*                 _seerConfigPage;
};

