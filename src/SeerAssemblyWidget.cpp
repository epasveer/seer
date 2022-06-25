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

    int space = fontMetrics().horizontalAdvance("Go to line ##") + 5;

    searchLineNumberLineEdit->setMaximumWidth(space);

    showSearchBar(false);      // Hide the search bar. ctrl+F to show it again.
    setSearchMatchCase(true);  // Search with case sensitivity.

    _textSearchShortcut     = new QShortcut(QKeySequence(tr("Ctrl+F")), this);
    _textSearchNextShortcut = new QShortcut(QKeySequence(tr("Ctrl+G")), this);
    _textSearchPrevShortcut = new QShortcut(QKeySequence(tr("Ctrl+Shift+G")), this);

    // Connect things.
    QObject::connect(searchTextLineEdit,                &QLineEdit::returnPressed,                      this,  &SeerAssemblyWidget::handleSearchTextLineEdit);
    QObject::connect(matchCaseCheckBox,                 &QCheckBox::stateChanged,                       this,  &SeerAssemblyWidget::handleSearchTextLineEdit);
    QObject::connect(searchDownToolButton,              &QToolButton::clicked,                          this,  &SeerAssemblyWidget::handleSearchDownToolButton);
    QObject::connect(searchUpToolButton,                &QToolButton::clicked,                          this,  &SeerAssemblyWidget::handleSearchUpToolButton);
    QObject::connect(searchLineNumberLineEdit,          &QLineEdit::returnPressed,                      this,  &SeerAssemblyWidget::handleSearchLineNumberLineEdit);
    QObject::connect(searchCloseToolButton,             &QToolButton::clicked,                          this,  &SeerAssemblyWidget::handleSearchCloseToolButton);
    QObject::connect(assemblyWidget,                    &SeerEditorWidgetAssemblyArea::showSearchBar,   this,  &SeerAssemblyWidget::showSearchBar);

    QObject::connect(_textSearchShortcut,               &QShortcut::activated,                          this,  &SeerAssemblyWidget::handleTextSearchShortcut);
    QObject::connect(_textSearchNextShortcut,           &QShortcut::activated,                          this,  &SeerAssemblyWidget::handleSearchDownToolButton);
    QObject::connect(_textSearchPrevShortcut,           &QShortcut::activated,                          this,  &SeerAssemblyWidget::handleSearchUpToolButton);

    // This is a hack to get at the QLineEdit's clear button.
    // QLineEdit doesn't have its own signal for this. Also, QLineEdit doesn't
    // emit a signal when the text goes blank. I don't want to handle each time
    // the text changes. Just when the user hits RETURN or when the text is cleared.
    QAction* clearAction = searchTextLineEdit->findChild<QAction*>();
    if (clearAction) {
        QObject::connect(clearAction, &QAction::triggered,   this, &SeerAssemblyWidget::handleClearSearchTextLineEdit);
    }
}

SeerAssemblyWidget::~SeerAssemblyWidget () {
}

SeerEditorWidgetAssemblyArea* SeerAssemblyWidget::assemblyArea () {
    return assemblyWidget;
}

bool SeerAssemblyWidget::isSearchBarShown () const {

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

bool SeerAssemblyWidget::searchMatchCase () const {

    return matchCaseCheckBox->isChecked();
}

void SeerAssemblyWidget::showSearchBar (bool flag) {

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

void SeerAssemblyWidget::setSearchMatchCase (bool flag) {

    matchCaseCheckBox->setChecked(flag);
}

void SeerAssemblyWidget::handleSearchLineNumberLineEdit () {

    QString str = searchLineNumberLineEdit->text();

    if (str == "") {
        return;
    }

    // Convert str to a gdb ready address. XXX

    QString address = str;

    searchLineNumberLineEdit->clear();

    assemblyArea()->scrollToLine(address);
}

void SeerAssemblyWidget::handleClearSearchTextLineEdit () {

    /*
     * We don't need to call the setText method to clear it.
     * The QLineEdit widget will do that on its own.
     searchTextLineEdit->setText("");
    */

    // Clear the matches label.
    matchesLabel->setText("");

    // Clear any previous highlights.
    assemblyArea()->clearFindText();
}

void SeerAssemblyWidget::handleSearchTextLineEdit () {

    QString str = searchTextLineEdit->text();

    matchesLabel->setText("");

    if (str == "") {
        assemblyArea()->clearFindText();
        return;
    }

    int nMatches = assemblyArea()->findText(str, (searchMatchCase() ? QTextDocument::FindCaseSensitively : QTextDocument::FindFlags()));

    matchesLabel->setText(QString("(%1)").arg(nMatches));
}

void SeerAssemblyWidget::handleSearchDownToolButton () {

    QString str = searchTextLineEdit->text();

    if (str == "") {
        return;
    }

    assemblyArea()->find(str, (searchMatchCase() ? QTextDocument::FindCaseSensitively : QTextDocument::FindFlags()));
}

void SeerAssemblyWidget::handleSearchUpToolButton () {

    QString str = searchTextLineEdit->text();

    if (str == "") {
        return;
    }

    assemblyArea()->find(str, (searchMatchCase() ? QTextDocument::FindCaseSensitively : QTextDocument::FindFlags()) | QTextDocument::FindBackward);
}

void SeerAssemblyWidget::handleSearchCloseToolButton () {
    showSearchBar(false);
}

void SeerAssemblyWidget::handleTextSearchShortcut () {

    if (isSearchBarShown() == true) {
        showSearchBar(false);
    }else{
        showSearchBar(true);
    }
}

