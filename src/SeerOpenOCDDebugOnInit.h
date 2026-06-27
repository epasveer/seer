#pragma once

#include <QtWidgets/QDialog>
#include <QtWidgets/QButtonGroup>
#include <QtCore/QString>
#include <QtCore/QStringList>

#include "ui_SeerOpenOCDDebugOnInit.h"

class SeerOpenOCDDebugOnInit : public QDialog, protected Ui::SeerOpenOCDDebugOnInit {

    Q_OBJECT

    public:
        explicit SeerOpenOCDDebugOnInit (QWidget* parent = 0);
       ~SeerOpenOCDDebugOnInit ();
        void                    setModuleName                       (const QString& name);
        const QString           moduleName                          ();
        void                    setCommandToTerm                    (const QString& command);
        const QString           commandToTerm                       ();
        void                    setkernelModuleSymbolPath           (const QString& path);
        const QString           kernelModuleSymbolPath              ();
        void                    setKernelModuleSourceCodePath       (const QString& path);
        const QString           kernelModuleSourceCodePath          ();
        void                    setSerialPortPath                   (const QString& path);
        const QString           serialPortPath                      ();

    private slots:
        void            handleKernelModuleSymbolButton              ();
        void            handleKernelModuleSourceCodeButton          ();
        void            onAccepted                                  ();
        void            handleComboBoxTextChanged                   ();

    private:
        QString         _moduleName;
        QString         _commandToTerm;
        QString         _kernelModuleSymbolPath;
        QString         _kernelModuleSourceCodePath;
        QString         _serialPortPath;


};

