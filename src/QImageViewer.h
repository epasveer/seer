#pragma once

#include <QtWidgets/QLabel>
#include <QtWidgets/QWidget>
#include <QtWidgets/QScrollArea>
#include <QtGui/QImage>
#include <QtGui/QKeyEvent>
#include <QtPrintSupport/QPrinter>

class QImageViewer : public QWidget {

    Q_OBJECT

    public:
        explicit QImageViewer (QWidget* parent = 0);
       ~QImageViewer ();

        bool                    loadFile                (const QString& file);
        void                    setImage                (const QImage& image);
        const QImage&           image                   () const;
        void                    setText                 (const QString& text);

        double                  zoomFactor              () const;

    public slots:
        void                    zoom                    (double factor);
        void                    zoomIn                  ();
        void                    zoomOut                 ();
        void                    zoomReset               ();
        void                    print                   ();

    protected slots:
        void                    keyPressEvent           (QKeyEvent* event);
        void                    enterEvent              (QEvent*    event);
        void                    leaveEvent              (QEvent*    event);

    private:
        QImage                  _image;
        QScrollArea*            _scrollArea;
        QLabel*                 _imageLabel;
        double                  _zoomFactor;
        QPrinter                _printer;
};

