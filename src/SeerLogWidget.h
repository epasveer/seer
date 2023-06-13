#pragma once

#include <QtWidgets/QWidget>
#include <QtCore/QString>
#include "ui_SeerLogWidget.h"

class SeerLogWidget : public QWidget, protected Ui::SeerLogWidgetForm {

    Q_OBJECT

    public:
        explicit SeerLogWidget (QWidget* parent = 0);
       ~SeerLogWidget ();

        virtual void        processText                 (const QString& text);

        bool                isLogEnabled                () const;
        void                setLogEnabled               (bool flag);
        void                moveToEnd                   ();
        void                setPlaceholderText          (const QString& text);

    signals:
        void                logEnabledChanged           (bool flag);

    public slots:
        void                handleText                  (const QString& text);
        void                handleClearButton           ();
        void                handlePrintButton           ();
        void                handleSaveButton            ();
        void                handleWrapTextCheckBox      ();
        void                handleEnableCheckBox        ();

    protected:
};

