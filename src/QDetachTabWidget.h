#pragma once
#include <QtWidgets/QTabWidget>
#include <QtWidgets/QPushButton>
#include <QtCore/QString>
#include <QtCore/QList>

struct QDetachTabInfo {

    QString     _title;
    QWidget*    _widget;
    QWidget*    _placeholderWidget;

    QDetachTabInfo(QString title, QWidget* widget, QWidget* placeholderWidget) : _title(title), _widget(widget), _placeholderWidget(placeholderWidget) {}
};


class QDetachTabWidget : public QTabWidget {

    Q_OBJECT

    public:
        QDetachTabWidget(QWidget* parent = 0);

        bool                                isDetached               (int tabIndex) const;

    public slots:
        void                                detachTab                (int tabIndex, Qt::WindowState windowState=Qt::WindowNoState);
        void                                reattachTab              (int tabIndex, Qt::WindowState windowState=Qt::WindowNoState);

    signals:
        void                                tabDetached              (int tabIndex);
        void                                tabReattached            (int tabIndex);

    protected:
        void                                closeEvent               (QCloseEvent* e);

    protected slots:
        void                                handleShowContextMenu    (const QPoint& point);
        void                                handleTabClosedRequested (int tabIndex);

    private:
        QList<QDetachTabInfo>::iterator     findWidget               (QWidget* widget);
        QList<QDetachTabInfo>::iterator     findPlaceholderWidget    (QWidget* widget);

        QList<QDetachTabInfo>               _tabInfo;
};

class QDetachTabWidgetPlaceholder : public QWidget {

    Q_OBJECT

    public:
        QDetachTabWidgetPlaceholder(int tabIndex, QWidget* parent = 0);

    signals:
        void                                reattach                 (int tabIndex);

    protected slots:
        void                                handlePushButtonClicked  ();

    private:
        QPushButton*                        _reattachPushButton;
        int                                 _tabIndex;
};

