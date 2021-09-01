#include "SeerEditorOptionsBarWidget.h"

SeerEditorOptionsBarWidget::SeerEditorOptionsBarWidget (QWidget* parent) : QWidget(parent) {

    // Construct the UI.
    setupUi(this);
}

SeerEditorOptionsBarWidget::~SeerEditorOptionsBarWidget () {
}

QToolButton* SeerEditorOptionsBarWidget::fileOpenToolButton () {
    return fileOpenTB;
}

QToolButton* SeerEditorOptionsBarWidget::textSearchToolButton () {
    return textSearchTB;
}

