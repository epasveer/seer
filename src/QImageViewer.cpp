#include "QImageViewer.h"
#include <QtWidgets/QScrollBar>
#include <QtWidgets/QVBoxLayout>
#include <QtWidgets/QFileDialog>
#include <QtGui/QImageReader>
#include <QtGui/QImageWriter>
#include <QtGui/QPainter>
#include <QtCore/QDebug>
#include <QtPrintSupport/QPrintDialog>

//
// https://doc.qt.io/qt-5/qtwidgets-widgets-imageviewer-example.html
// https://stackoverflow.com/questions/53193010/how-to-resize-a-qlabel-with-pixmap-inside-a-qscrollarea
//

QImageViewer::QImageViewer (QWidget* parent) : QWidget(parent) {

    _zoomFactor = 1.0;

    // Setup the widgets
    QVBoxLayout* layout = new QVBoxLayout(this);
    layout->setContentsMargins(0, 0, 0, 0);

    _imageLabel = new QLabel();
    _imageLabel->setBackgroundRole(QPalette::Base);
    _imageLabel->setScaledContents(true);

    _scrollArea = new QScrollArea(this);
    _scrollArea->setBackgroundRole(QPalette::Dark);
    _scrollArea->setWidget(_imageLabel);
    _scrollArea->setWidgetResizable(false);

    layout->addWidget(_scrollArea);

    _imageLabel->setPixmap(QPixmap::fromImage(_image));
}

QImageViewer::~QImageViewer () {
}

bool QImageViewer::loadFile (const QString& file) {

    QImageReader reader(file);

    reader.setAutoTransform(true);

    QImage image = reader.read();

    if (image.isNull()) {
        return false;
    }

    setImage(image);

    return true;
}

bool QImageViewer::saveFile (const QString& file) {

    if (file.isEmpty() == true) {
        return false;
    }

    QImageWriter writer(file);

    bool f = writer.write(_image);

    if (f == false) {
        qDebug() << writer.errorString();
    }

    return f;
}

bool QImageViewer::saveFileDialog (const QString& file) {

    QString fileName = QFileDialog::getSaveFileName(this, tr("Save Image File"), file, tr("Image Files (*.png *.jpg *.bmp)"), nullptr, QFileDialog::DontUseNativeDialog);

    return saveFile(fileName);
}

void QImageViewer::setImage (const QImage& image) {

    //qDebug() << image;

    _image      = image.copy();
    _zoomFactor = 1.0;

    _imageLabel->setPixmap(QPixmap::fromImage(_image));

    zoomReset();
}

const QImage& QImageViewer::image () const {

    return _image;
}

void QImageViewer::setText (const QString& text) {

    _imageLabel->setText(text);
}

double QImageViewer::zoomFactor () const {

    return _zoomFactor;
}

void QImageViewer::zoom (double factor) {

    _zoomFactor = factor;

#if QT_VERSION >= QT_VERSION_CHECK(5, 15, 0)
    _imageLabel->resize(_zoomFactor * _imageLabel->pixmap(Qt::ReturnByValue).size());
#else
    _imageLabel->resize(_zoomFactor * _imageLabel->pixmap()->size());
#endif
}

void QImageViewer::zoomIn () {

    zoom(zoomFactor() * 1.25);
}

void QImageViewer::zoomOut () {

    zoom(zoomFactor() * 0.8);
}

void QImageViewer::zoomReset () {

    zoom(1.0);
}

void QImageViewer::print () {

    QPrintDialog dialog(&_printer, this);

#if QT_VERSION >= QT_VERSION_CHECK(5, 15, 0)
    if (dialog.exec()) {
        QPainter painter(&_printer);
        QRect rect = painter.viewport();
        QSize size = _imageLabel->pixmap(Qt::ReturnByValue).size();
        size.scale(rect.size(), Qt::KeepAspectRatio);
        painter.setViewport(rect.x(), rect.y(), size.width(), size.height());
        painter.setWindow(_imageLabel->pixmap(Qt::ReturnByValue).rect());
        painter.drawPixmap(0, 0, _imageLabel->pixmap(Qt::ReturnByValue));
    }
#else
    if (dialog.exec()) {
        QPainter painter(&_printer);
        QRect rect = painter.viewport();
        QSize size = _imageLabel->pixmap()->size();
        size.scale(rect.size(), Qt::KeepAspectRatio);
        painter.setViewport(rect.x(), rect.y(), size.width(), size.height());
        painter.setWindow(_imageLabel->pixmap()->rect());
        painter.drawPixmap(0, 0, *_imageLabel->pixmap());
    }
#endif
}

void QImageViewer::keyPressEvent (QKeyEvent* event) {

    //qDebug() << "Key =" << event->key();

    switch (event->key()) {
        case Qt::Key_Escape:
            zoomReset();
            break;
        case Qt::Key_Plus:
            zoomIn();
            break;
        case Qt::Key_Minus:
            zoomOut();
            break;
        default:
            QWidget::keyPressEvent(event);
            break;
    }
}

void QImageViewer::enterEvent (QEvent* event) {

    Q_UNUSED(event);

    setFocus();
}

void QImageViewer::leaveEvent (QEvent* event) {

    Q_UNUSED(event);
}

