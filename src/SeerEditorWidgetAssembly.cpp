#include "SeerEditorWidgetAssembly.h"
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

SeerEditorWidgetAssembly::SeerEditorWidgetAssembly(QWidget* parent) : QWidget(parent) {

    // Construct the UI.
    setupUi(this);

    // Set the widgets.
    assemblyArea()->show();
    searchTextLineEdit->enableReturnPressedOnClear();

    showSearchBar(false);      // Hide the search bar. ctrl+F to show it again.
    setSearchMatchCase(true);  // Search with case sensitivity.

    _textSearchShortcut     = new QShortcut(QKeySequence(tr("Ctrl+F")), this);
    _textSearchNextShortcut = new QShortcut(QKeySequence(tr("Ctrl+G")), this);
    _textSearchPrevShortcut = new QShortcut(QKeySequence(tr("Ctrl+Shift+G")), this);

    setKeySettings(SeerKeySettings::populate());

    // Connect things.
    QObject::connect(searchTextLineEdit,                &QLineEdit::returnPressed,                      this,  &SeerEditorWidgetAssembly::handleSearchTextLineEdit);
    QObject::connect(matchCaseCheckBox,                 &QCheckBox::stateChanged,                       this,  &SeerEditorWidgetAssembly::handleSearchTextLineEdit);
    QObject::connect(searchDownToolButton,              &QToolButton::clicked,                          this,  &SeerEditorWidgetAssembly::handleSearchDownToolButton);
    QObject::connect(searchUpToolButton,                &QToolButton::clicked,                          this,  &SeerEditorWidgetAssembly::handleSearchUpToolButton);
    QObject::connect(searchLineNumberLineEdit,          &QLineEdit::returnPressed,                      this,  &SeerEditorWidgetAssembly::handleSearchLineNumberLineEdit);
    QObject::connect(searchCloseToolButton,             &QToolButton::clicked,                          this,  &SeerEditorWidgetAssembly::handleSearchCloseToolButton);
    QObject::connect(assemblyWidget,                    &SeerEditorWidgetAssemblyArea::showSearchBar,   this,  &SeerEditorWidgetAssembly::showSearchBar);

    QObject::connect(_textSearchShortcut,               &QShortcut::activated,                          this,  &SeerEditorWidgetAssembly::handleTextSearchShortcut);
    QObject::connect(_textSearchNextShortcut,           &QShortcut::activated,                          this,  &SeerEditorWidgetAssembly::handleSearchDownToolButton);
    QObject::connect(_textSearchPrevShortcut,           &QShortcut::activated,                          this,  &SeerEditorWidgetAssembly::handleSearchUpToolButton);
}

SeerEditorWidgetAssembly::~SeerEditorWidgetAssembly () {
}

SeerEditorWidgetAssemblyArea* SeerEditorWidgetAssembly::assemblyArea () {
    return assemblyWidget;
}

bool SeerEditorWidgetAssembly::isSearchBarShown () const {

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

bool SeerEditorWidgetAssembly::searchMatchCase () const {

    return matchCaseCheckBox->isChecked();
}

void SeerEditorWidgetAssembly::setKeySettings (const SeerKeySettings& settings) {

    _keySettings = settings;

    if (_keySettings.has("SearchText") == true) {
        _textSearchShortcut->setKey(_keySettings.get("SearchText")._sequence);
    }

    if (_keySettings.has("SearchTextNext") == true) {
        _textSearchNextShortcut->setKey(_keySettings.get("SearchTextNext")._sequence);
    }

    if (_keySettings.has("SearchTextPrev") == true) {
        _textSearchPrevShortcut->setKey(_keySettings.get("SearchTextPrev")._sequence);
    }
}

const SeerKeySettings& SeerEditorWidgetAssembly::keySettings () const {

    return _keySettings;
}

void SeerEditorWidgetAssembly::showSearchBar (bool flag) {

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

void SeerEditorWidgetAssembly::setSearchMatchCase (bool flag) {

    matchCaseCheckBox->setChecked(flag);
}

void SeerEditorWidgetAssembly::handleSearchLineNumberLineEdit () {

    QString address = searchLineNumberLineEdit->text();

    if (address == "") {
        return;
    }

    searchLineNumberLineEdit->clear();

    assemblyArea()->scrollToLine(address);
}

void SeerEditorWidgetAssembly::handleSearchTextLineEdit () {

    QString str = searchTextLineEdit->text();

    matchesLabel->setText("");

    if (str == "") {
        assemblyArea()->clearFindText();
        return;
    }

    int nMatches = assemblyArea()->findText(str, (searchMatchCase() ? QTextDocument::FindCaseSensitively : QTextDocument::FindFlags()));

    matchesLabel->setText(QString("(%1)").arg(nMatches));
}

void SeerEditorWidgetAssembly::handleSearchDownToolButton () {

    QString str = searchTextLineEdit->text();

    if (str == "") {
        return;
    }

    assemblyArea()->find(str, (searchMatchCase() ? QTextDocument::FindCaseSensitively : QTextDocument::FindFlags()));
}

void SeerEditorWidgetAssembly::handleSearchUpToolButton () {

    QString str = searchTextLineEdit->text();

    if (str == "") {
        return;
    }

    assemblyArea()->find(str, (searchMatchCase() ? QTextDocument::FindCaseSensitively : QTextDocument::FindFlags()) | QTextDocument::FindBackward);
}

void SeerEditorWidgetAssembly::handleSearchCloseToolButton () {
    showSearchBar(false);
}

void SeerEditorWidgetAssembly::handleTextSearchShortcut () {

    if (isSearchBarShown() == true) {
        showSearchBar(false);
    }else{
        showSearchBar(true);
    }
}

