#include "SeerEditorWidget.h"
#include <QtGui/QColor>
#include <QtGui/QPainter>
#include <QtGui/QTextBlock>
#include <QtGui/QFont>
#include <QtGui/QIcon>
#include <QtGui/QRadialGradient>
#include <QtWidgets/QScrollBar>
#include <QtWidgets/QFileDialog>
#include <QtWidgets/QMenu>
#include <QtWidgets/QAction>
#include <QtGui/QTextCursor>
#include <QtGui/QPalette>
#include <QtCore/QList>
#include <QtCore/QString>
#include <QtCore/QTextStream>
#include <QtCore/QFile>
#include <QtCore/QDebug>

SeerAssemblyWidget::SeerAssemblyWidget(QWidget* parent) : QWidget(parent) {

    // Construct the UI.
    setupUi(this);

    // Set the widgets.
    assemblyArea()->show(); // XXX Why is this needed?  SeerEditorWidget doesn't need it.

    // Connect things.
}

SeerAssemblyWidget::~SeerAssemblyWidget () {
}

SeerEditorWidgetAssemblyArea* SeerAssemblyWidget::assemblyArea () {
    return assemblyWidget;
}

