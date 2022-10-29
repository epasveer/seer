#include <string>
#include <iostream>
#include <fstream>
#include <filesystem>
#include <QtGui/QImageReader>
#include <QtCore/QDebug>

int main (int argc, char** argv) {

    std::cout << "Hello, Image!" << std::endl;

    QImageReader reader("lena.bmp");
    reader.setAutoTransform(true);

    QImage image = reader.read();
    if (image.isNull()) {
        return false;
    }

    QImage image_rgb  = image.convertToFormat(QImage::Format_RGB888);
    QImage image_rgba = image.convertToFormat(QImage::Format_RGBA8888);

    qDebug() << image;
    qDebug() << image_rgb;
    qDebug() << image_rgba;

    // QImage(QSize(512, 512),format=QImage::Format_Indexed8,depth=8,colorCount=256,devicePixelRatio=1,bytesPerLine=512,sizeInBytes=262144)
    // QImage(QSize(512, 512),format=QImage::Format_RGB888,depth=24,devicePixelRatio=1,bytesPerLine=1536,sizeInBytes=786432)
    // QImage(QSize(512, 512),format=QImage::Format_RGBA8888,depth=32,devicePixelRatio=1,bytesPerLine=2048,sizeInBytes=1048576)

    const uchar* bits      = image.bits();
    const uchar* bits_rgb  = image_rgb.bits();
    const uchar* bits_rgba = image_rgba.bits();

    return 0;
}

