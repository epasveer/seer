#include <string>
#include <iostream>
#include <fstream>
#include <filesystem>
#include <QtGui/QImageReader>
#include <QtCore/QDebug>

int main (int argc, char** argv) {

    std::cout << "Hello, Image!" << std::endl;

    QImageReader reader("mona.jpg");
    reader.setAutoTransform(true);

    QImage image = reader.read();
    if (image.isNull()) {
        return false;
    }

    QImage image_rgb  = image.convertToFormat(QImage::Format_RGB888);
    QImage image_rgba = image.convertToFormat(QImage::Format_RGBA8888);

    qInfo() << image;
    qInfo() << image_rgb;
    qInfo() << image_rgba;

    // QImage(QSize(220, 197),format=QImage::Format_RGB32,depth=32,devicePixelRatio=1,bytesPerLine=880,sizeInBytes=173360)
    // QImage(QSize(220, 197),format=QImage::Format_RGB888,depth=24,devicePixelRatio=1,bytesPerLine=660,sizeInBytes=130020)
    // QImage(QSize(220, 197),format=QImage::Format_RGBA8888,depth=32,devicePixelRatio=1,bytesPerLine=880,sizeInBytes=173360)

    const uchar* bits      = image.bits();
    const uchar* bits_rgb  = image_rgb.bits();
    const uchar* bits_rgba = image_rgba.bits();

    return 0;
}

