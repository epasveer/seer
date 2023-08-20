#pragma once

#include <QtWidgets/QWidget>
#include <QtCore/QString>
#include <QtCore/QStringList>
#include "ui_SeerSourceBrowserWidget.h"

class SeerSourceBrowserWidget : public QWidget, protected Ui::SeerSourceBrowserWidgetForm {

    Q_OBJECT

    public:
        explicit SeerSourceBrowserWidget (QWidget* parent = 0);
       ~SeerSourceBrowserWidget ();

        void                        setMiscFilePatterns         (const QStringList& patterns);
        const QStringList&          miscFilePatterns            () const;

        void                        setSourceFilePatterns       (const QStringList& patterns);
        const QStringList&          sourceFilePatterns          () const;

        void                        setHeaderFilePatterns       (const QStringList& patterns);
        const QStringList&          headerFilePatterns          () const;

        void                        setIgnoreFilePatterns       (const QStringList& patterns);
        const QStringList&          ignoreFilePatterns          () const;

    public slots:
        void                        handleText                  (const QString& text);
        void                        refresh                     ();

    protected slots:
        void                        handleSearchLineEdit        (const QString& text);
        void                        handleItemDoubleClicked     (QTreeWidgetItem* item, int column);
        void                        handleItemEntered           (QTreeWidgetItem* item, int column);

    signals:
        void                        refreshSourceList           ();
        void                        selectedFile                (QString file, QString fullname, int lineno);

    protected:
    private:
        QTreeWidgetItem*            _sourceFilesItems;
        QTreeWidgetItem*            _headerFilesItems;
        QTreeWidgetItem*            _miscFilesItems;
        QStringList                 _sourceFilePatterns;
        QStringList                 _headerFilePatterns;
        QStringList                 _miscFilePatterns;
        QStringList                 _ignoreFilePatterns;
};

