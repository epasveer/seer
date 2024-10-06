#pragma once

#include <QtGui/QPaintEvent>
#include <QtGui/QColor>
#include <QtWidgets/QFrame>
#include <QtWidgets/QWidget>

class QColorSwatch : public QWidget {

    public:
        QColorSwatch(QWidget* parent = 0);
        QColorSwatch(const QColor& color, QWidget* parent = 0);
       ~QColorSwatch();

        void                setColor                (const QColor& color);
        QColor              color                   () const;

    protected:
        void                paintEvent              (QPaintEvent* e);

    private:
        QColor              _color;
};

