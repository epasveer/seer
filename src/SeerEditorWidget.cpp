#include "SeerEditorWidget.h"
#include <QtGui/QColor>
#include <QtGui/QPainter>
#include <QtGui/QTextBlock>
#include <QtGui/QFont>
#include <QtGui/QIcon>
#include <QtGui/QRadialGradient>
#include <QtWidgets/QScrollBar>
#include <QtWidgets/QMenu>
#include <QtWidgets/QAction>
#include <QtGui/QTextCursor>
#include <QtGui/QPalette>
#include <QtCore/QList>
#include <QtCore/QString>
#include <QtCore/QTextStream>
#include <QtCore/QFile>
#include <QtCore/QDebug>

SeerEditorWidget::SeerEditorWidget(QWidget *parent) : QWidget(parent) {

    // Construct the UI.
    setupUi(this);

    // Set the widgets.
    int space = fontMetrics().horizontalAdvance("Go to line ##") + 5;

    lineNumberLineEdit->setMaximumWidth(space);

    showSearchBar(false);    // Hide the search bar. ctrl+F to show it again.
    showAlternateBar(false); // Hide the alternate bar. ctrl+O to show it again.

    // Connect things.
    QObject::connect(searchTextLineEdit,    &QLineEdit::returnPressed,                      this,  &SeerEditorWidget::handleSearchTextLineEdit);
    QObject::connect(searchDownToolButton,  &QToolButton::clicked,                          this,  &SeerEditorWidget::handleSearchDownToolButton);
    QObject::connect(searchUpToolButton,    &QToolButton::clicked,                          this,  &SeerEditorWidget::handleSearchUpToolButton);
    QObject::connect(lineNumberLineEdit,    &QLineEdit::returnPressed,                      this,  &SeerEditorWidget::handleLineNumberLineEdit);
    QObject::connect(closeToolButton,       &QToolButton::clicked,                          this,  &SeerEditorWidget::handleCloseToolButton);
    QObject::connect(sourceWidget,          &SeerEditorWidgetSourceArea::showSearchBar,     this,  &SeerEditorWidget::showSearchBar);
    QObject::connect(sourceWidget,          &SeerEditorWidgetSourceArea::showAlternateBar,  this,  &SeerEditorWidget::showAlternateBar);
}

SeerEditorWidget::~SeerEditorWidget () {
}

SeerEditorWidgetSourceArea* SeerEditorWidget::sourceArea () {
    return sourceWidget;
}

void SeerEditorWidget::handleLineNumberLineEdit () {

    QString str = lineNumberLineEdit->text();

    if (str == "") {
        return;
    }

    int lineno = str.toInt();

    if (lineno < 1) {
        return;
    }

    lineNumberLineEdit->clear();

    sourceArea()->scrollToLine(lineno);
}

void SeerEditorWidget::handleSearchTextLineEdit () {

    QString str = searchTextLineEdit->text();

    if (str == "") {
        return;
    }

    sourceArea()->find(str, QTextDocument::FindCaseSensitively);
}

void SeerEditorWidget::handleSearchDownToolButton () {

    QString str = searchTextLineEdit->text();

    if (str == "") {
        return;
    }

    sourceArea()->find(str, QTextDocument::FindCaseSensitively);
}

void SeerEditorWidget::handleSearchUpToolButton () {

    QString str = searchTextLineEdit->text();

    if (str == "") {
        return;
    }

    sourceArea()->find(str, QTextDocument::FindCaseSensitively|QTextDocument::FindBackward);
}

void SeerEditorWidget::showSearchBar (bool flag) {

    // Go through the widgets in the search bar and hide/show them.
    for (int i = 0; i != searchBarLayout->count(); ++i) {
        QWidget* w = searchBarLayout->itemAt(i)->widget();
        if (w != 0) {
            w->setVisible(flag);
        }
    }

    // If 'show', give the searchTextLineEdit the focus.
    if (flag) {
        searchTextLineEdit->setFocus(Qt::MouseFocusReason);
    }

    // Update the layout.
    searchBarLayout->update();
}

bool SeerEditorWidget::isSearchBarShown () const {

    bool shown = false;

    // Go through the widgets in the search bar to see if any are visable.
    for (int i = 0; i != searchBarLayout->count(); ++i) {
        QWidget* w = searchBarLayout->itemAt(i)->widget();
        if (w != 0) {
            if (w->isVisible()) {
                shown = true;
            }
        }
    }

    return shown;
}

void SeerEditorWidget::showAlternateBar (bool flag) {

    // Go through the widgets in the search bar and hide/show them.
    for (int i = 0; i != alternateBarLayout->count(); ++i) {
        QWidget* w = alternateBarLayout->itemAt(i)->widget();
        if (w != 0) {
            w->setVisible(flag);
        }
    }

    // If 'show', give the searchTextLineEdit the focus.
    if (flag) {
        alternateLineEdit->setFocus(Qt::MouseFocusReason);
    }

    // Update the layout.
    alternateBarLayout->update();
}

bool SeerEditorWidget::isAlternateBarShown () const {

    bool shown = false;

    // Go through the widgets in the search bar to see if any are visable.
    for (int i = 0; i != alternateBarLayout->count(); ++i) {
        QWidget* w = alternateBarLayout->itemAt(i)->widget();
        if (w != 0) {
            if (w->isVisible()) {
                shown = true;
            }
        }
    }

    return shown;
}

void SeerEditorWidget::handleCloseToolButton () {
    showSearchBar(false);
}

