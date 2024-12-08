#pragma once

#include <QtGui/QMouseEvent>
#include <QtGui/QPaintEvent>
#include <QtGui/QColor>
#include <QtWidgets/QFrame>
#include <QtWidgets/QWidget>
#include "ui_QColorButton.h"

class QColorButton : public QFrame, protected Ui::QColorButtonForm {

        Q_OBJECT

    public:
        QColorButton(QWidget* parent = 0);
        QColorButton(const QColor& color, QWidget* parent = 0);
       ~QColorButton();

        void                setColor                (const QColor& color);
        QColor              color                   () const;

    signals:
        void                colorChanged            ();

    protected:
        void                mouseDoubleClickEvent   (QMouseEvent* e);
};

