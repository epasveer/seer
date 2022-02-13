#pragma once
#include <QtWidgets/QTabWidget>
#include <QtCore/QString>
#include <QtCore/QList>

struct QDetachTabInfo {

    QDetachTabInfo(QString title, QWidget* widget, QWidget* placeholderWidget) : _title(title), _widget(widget), _placeholderWidget(placeholderWidget) {}

    QWidget*    _widget;
    QWidget*    _placeholderWidget;
    QString     _title;
};


class QDetachTabWidget : public QTabWidget {

    Q_OBJECT

    public:
        QDetachTabWidget(QWidget* parent = 0);

    protected:
        void                                closeEvent               (QCloseEvent* e);

    protected slots:
        void                                handleShowContextMenu    (int tabIndex);
        void                                handleTabClosedRequested (int tabIndex);

    private:
        QList<QDetachTabInfo>::iterator     findWidget               (QWidget* widget);
        QList<QDetachTabInfo>::iterator     findPlaceholderWidget    (QWidget* widget);

        QList<QDetachTabInfo>               _tabInfo;
};

