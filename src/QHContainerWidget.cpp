// SPDX-FileCopyrightText: 2021 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: MIT

#include "QHContainerWidget.h"

QHContainerWidget::QHContainerWidget (QWidget* parent) : QWidget(parent) {

    // Construct the UI.
    setupUi(this);

    // Setup the widgets
    // Connect things.
}

QHContainerWidget::~QHContainerWidget () {
}

void QHContainerWidget::setSpacing (int spacing) {

    horizontalLayout->setSpacing(spacing);
}

void QHContainerWidget::addWidget (QWidget* widget) {

    horizontalLayout->addWidget(widget);
}

void QHContainerWidget::removeWidget (QWidget* widget) {

    horizontalLayout->removeWidget(widget);
}

