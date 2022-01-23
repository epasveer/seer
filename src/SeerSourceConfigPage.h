#pragma once

#include <QtWidgets/QWidget>
#include <QtCore/QString>
#include <QtCore/QStringList>

#include "ui_SeerSourceConfigPage.h"

class SeerSourceConfigPage : public QWidget, public Ui::SeerSourceConfigPage {

    Q_OBJECT

    public:
        explicit SeerSourceConfigPage (QWidget* parent = 0);
       ~SeerSourceConfigPage ();

        void                                setAlternateDirectories         (const QStringList& alternateDirectories);
        QStringList                         alternateDirectories            () const;

    protected slots:
        void                                handleAddButtonClicked          ();
        void                                handleUpButtonClicked           ();
        void                                handleDownButtonClicked         ();
        void                                handleDeleteButtonClicked       ();

    private:
};

