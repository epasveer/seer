#include "SeerProgressIndicator.h"
#include <QtWidgets/QMenu>
#include <QAction>
#include <QtGui/QCursor>
#include <QtCore/QSettings>
#include <QtCore/QDebug>

SeerProgressIndicator::SeerProgressIndicator(QWidget* parent) : QProgressIndicator(parent) {

    setContextMenuPolicy(Qt::CustomContextMenu);

    QObject::connect(this, &SeerProgressIndicator::customContextMenuRequested,    this, &SeerProgressIndicator::handleShowContextMenu);

    // Read settings.
    readSettings();
}

SeerProgressIndicator::~SeerProgressIndicator() {
}

void SeerProgressIndicator::writeSettings () {

    QSettings settings;

    settings.beginGroup("progressindicator"); {
        settings.setValue("type", typeName());
    } settings.endGroup();
}

void SeerProgressIndicator::readSettings () {

    QSettings settings;

    settings.beginGroup("progressindicator"); {
        setType(settings.value("type", QString("BallRotate")).toString());
    } settings.endGroup();
}

void SeerProgressIndicator::handleShowContextMenu (const QPoint& point) {

    // Don't do anything.
    if (point.isNull()) {
        return;
    }

    // Possible indicator types.
    QStringList typeList = types();

    if (typeList.size() == 0) {
        return;
    }

    // Build up menu.
    QMenu menu("Progress Indicators", this);

    for (int i=0; i<typeList.size(); i++) {
        menu.addAction(typeList[i]);
    }

    // Exec the menu
    QAction* action = menu.exec(QCursor::pos());

    if (action == 0) {
        return;
    }

    // Set the new indicator type.
    setType(action->text());

    // Save it for next time.
    writeSettings();
}

