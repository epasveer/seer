#include "SeerImageWidget.h"
#include "SeerUtl.h"
#include <QtGui/QIntValidator>
#include <QtCore/QDebug>

SeerImageWidget::SeerImageWidget(QWidget* parent) : QWidget(parent) {

    // Construct the UI.
    setupUi(this);

    // Setup the widgets
    widthLineEdit->setValidator(new QIntValidator(1, 9999999, this));
    heightLineEdit->setValidator(new QIntValidator(1, 9999999, this));

    // Connect things.
    QObject::connect(redrawToolButton,  &QToolButton::clicked,    this,  &SeerImageWidget::handleRedrawToolButton);
}

SeerImageWidget::~SeerImageWidget() {
}

void SeerImageWidget::setData (const QByteArray& data) {

    _data = data;

    refresh();
}

void SeerImageWidget::refresh () {

    // Clear the status.
    statusLineEdit->setText("");

    // Nothing to display?
    if (_data.size() < 1) {
        return;
    }

    // Get image dimensions.
    int w = widthLineEdit->text().toInt();
    int h = heightLineEdit->text().toInt();

    if (w < 1 || h < 1) {
        return;
    }

    // Get image format.
    QString format = formatComboBox->currentText();

    // Validate the image dimensions vs. the data size.
    int bytes = 0;

    if (format == "RGBA8888") {
        bytes = w * h * 4;
    }else if (format == "RGB888") {
        bytes = w * h * 3;
    }else{
        bytes = -1;
    }

    // Unknown format.
    if (bytes < 0) {
        statusLineEdit->setText("Unknown image format of '" + format + ".");
        return;
    }

    // Data is not large enough for requested image dimensions.
    if (bytes > _data.size()) {
        statusLineEdit->setText(QString("The %1 memory bytes is not enough for the '%2' image (%3x%4). Need at least %5 bytes.").arg(_data.size())
                                                                                                                             .arg(format)
                                                                                                                             .arg(w)
                                                                                                                             .arg(h)
                                                                                                                             .arg(bytes));
        return;
    }

    // Construct image.
    QImage image;

    if (format == "RGBA8888") {
        image = QImage((const uchar*)_data.data(), w, h, QImage::Format_RGBA8888);
    }else if (format == "RGB888") {
        image = QImage((const uchar*)_data.data(), w, h, QImage::Format_RGB888);
    }

    imageViewer->setImage(image);
}

void SeerImageWidget::handleRedrawToolButton () {

    refresh();
}

