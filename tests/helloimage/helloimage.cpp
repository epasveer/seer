#include <string>
#include <iostream>
#include <fstream>
#include <filesystem>
#include <QtGui/QImageReader>
#include <QtCore/QDebug>

class LocalImage {
    public:
        LocalImage ();
        LocalImage (const QImage& image);

        void            setImage    (const QImage& image);
        const uchar*    bits        () const;
        int             width       () const;
        int             height      () const;

    private:
        QImage _image;
};

LocalImage::LocalImage() {
}

LocalImage::LocalImage(const QImage& image) {
    setImage(image);
};

void LocalImage::setImage (const QImage& image) {
    _image = image;
};

const uchar* LocalImage::bits() const {
    return _image.bits();
};

int LocalImage::width() const {
    return _image.width();
}

int LocalImage::height() const {
    return _image.height();
}


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

    LocalImage img(image);
    qInfo() << img.width();
    qInfo() << img.height();

    return 0;
}

