#pragma once

#include <QtGui/QMouseEvent>
#include <QtGui/QPaintEvent>
#include <QtGui/QColor>
#include <QtWidgets/QWidget>

class QColorButton : public QWidget {

        Q_OBJECT

    public:
        QColorButton(QWidget* parent = 0);
        QColorButton(const QColor& color, QWidget* parent = 0);
       ~QColorButton();

        void                setColor                (const QColor &color);
        const QColor&       color                   ();

    signals:
        void                colorChanged            ();

    protected:
        void                mouseDoubleClickEvent   (QMouseEvent* e);
        void                paintEvent              (QPaintEvent* e);

    private slots:

    private:
        QColor              _color;
};

