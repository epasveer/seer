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

SeerEditorWidget::SeerEditorWidget(QWidget* parent) : QWidget(parent) {

    // Construct the UI.
    setupUi(this);

    // Set the widgets.
    int space = fontMetrics().horizontalAdvance("Go to line ##") + 5;

    searchLineNumberLineEdit->setMaximumWidth(space);

    showSearchBar(false);      // Hide the search bar. ctrl+F to show it again.
    showAlternateBar(false);   // Hide the alternate bar. ctrl+O to show it again.
    setSearchMatchCase(true);  // Search with case sensitivity.

    _textSearchShortcut     = new QShortcut(QKeySequence(tr("Ctrl+F")), this);
    _textSearchNextShortcut = new QShortcut(QKeySequence(tr("Ctrl+G")), this);
    _textSearchPrevShortcut = new QShortcut(QKeySequence(tr("Ctrl+Shift+G")), this);
    _alternateDirShortcut   = new QShortcut(QKeySequence(tr("Ctrl+O")), this);

    setKeySettings(SeerKeySettings::populate());

    // Connect things.
    QObject::connect(searchTextLineEdit,                &QLineEdit::returnPressed,                      this,  &SeerEditorWidget::handleSearchTextLineEdit);
    QObject::connect(matchCaseCheckBox,                 &QCheckBox::stateChanged,                       this,  &SeerEditorWidget::handleSearchTextLineEdit);
    QObject::connect(searchDownToolButton,              &QToolButton::clicked,                          this,  &SeerEditorWidget::handleSearchDownToolButton);
    QObject::connect(searchUpToolButton,                &QToolButton::clicked,                          this,  &SeerEditorWidget::handleSearchUpToolButton);
    QObject::connect(searchLineNumberLineEdit,          &QLineEdit::returnPressed,                      this,  &SeerEditorWidget::handleSearchLineNumberLineEdit);
    QObject::connect(searchCloseToolButton,             &QToolButton::clicked,                          this,  &SeerEditorWidget::handleSearchCloseToolButton);
    QObject::connect(alternateCloseToolButton,          &QToolButton::clicked,                          this,  &SeerEditorWidget::handleAlternateCloseToolButton);
    QObject::connect(alternateFileOpenToolButton,       &QToolButton::clicked,                          this,  &SeerEditorWidget::handleAlternateFileOpenToolButton);
    QObject::connect(alternateAddToGlobalToolButton,    &QToolButton::clicked,                          this,  &SeerEditorWidget::handleAlternateAddToGlobalToolButton);
    QObject::connect(alternateLineEdit,                 &QLineEdit::returnPressed,                      this,  &SeerEditorWidget::handleAlternateLineEdit);
    QObject::connect(sourceWidget,                      &SeerEditorWidgetSourceArea::showSearchBar,     this,  &SeerEditorWidget::showSearchBar);
    QObject::connect(sourceWidget,                      &SeerEditorWidgetSourceArea::showAlternateBar,  this,  &SeerEditorWidget::showAlternateBar);

    QObject::connect(_textSearchShortcut,               &QShortcut::activated,                          this,  &SeerEditorWidget::handleTextSearchShortcut);
    QObject::connect(_textSearchNextShortcut,           &QShortcut::activated,                          this,  &SeerEditorWidget::handleSearchDownToolButton);
    QObject::connect(_textSearchPrevShortcut,           &QShortcut::activated,                          this,  &SeerEditorWidget::handleSearchUpToolButton);
    QObject::connect(_alternateDirShortcut,             &QShortcut::activated,                          this,  &SeerEditorWidget::handleAlternateDirectoryShortcut);

    // This is a hack to get at the QLineEdit's clear button.
    // QLineEdit doesn't have its own signal for this. Also, QLineEdit doesn't
    // emit a signal when the text goes blank. I don't want to handle each time
    // the text changes. Just when the user hits RETURN or when the text is cleared.
    QAction* clearAction = searchTextLineEdit->findChild<QAction*>();
    if (clearAction) {
        QObject::connect(clearAction, &QAction::triggered,   this, &SeerEditorWidget::handleClearSearchTextLineEdit);
    }
}

SeerEditorWidget::~SeerEditorWidget () {
}

SeerEditorWidgetSourceArea* SeerEditorWidget::sourceArea () {
    return sourceWidget;
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

bool SeerEditorWidget::searchMatchCase () const {

    return matchCaseCheckBox->isChecked();
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

void SeerEditorWidget::setKeySettings (const SeerKeySettings& settings) {

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

    if (_keySettings.has("AlternateDir") == true) {
        _alternateDirShortcut->setKey(_keySettings.get("AlternateDir")._sequence);
    }
}

const SeerKeySettings& SeerEditorWidget::keySettings () const {

    return _keySettings;
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

void SeerEditorWidget::setSearchMatchCase (bool flag) {

    matchCaseCheckBox->setChecked(flag);
}

void SeerEditorWidget::showAlternateBar (bool flag) {

    // Set the label's text.
    alternateLabel->setText("Enter directory path for '" + sourceArea()->file() + "'");

    // Go through the widgets in the search bar and hide/show them.
    for (int i = 0; i != alternateBarLayout->count(); ++i) {
        QWidget* w = alternateBarLayout->itemAt(i)->widget();
        if (w != 0) {
            w->setVisible(flag);
        }
    }

    // If 'show', give the alternateLineEdit the focus.
    if (flag) {
        alternateLineEdit->setFocus(Qt::MouseFocusReason);
    }

    // Update the layout.
    alternateBarLayout->update();
}

void SeerEditorWidget::handleSearchLineNumberLineEdit () {

    QString str = searchLineNumberLineEdit->text();

    if (str == "") {
        return;
    }

    int lineno = str.toInt();

    if (lineno < 1) {
        return;
    }

    searchLineNumberLineEdit->clear();

    sourceArea()->scrollToLine(lineno);
}

void SeerEditorWidget::handleClearSearchTextLineEdit () {

    /*
     * We don't need to call the setText method to clear it.
     * The QLineEdit widget will do that on its own.
     searchTextLineEdit->setText("");
    */

    // Clear the matches label.
    matchesLabel->setText("");

    // Clear any previous highlights.
    sourceArea()->clearFindText();
}

void SeerEditorWidget::handleSearchTextLineEdit () {

    QString str = searchTextLineEdit->text();

    matchesLabel->setText("");

    if (str == "") {
        sourceArea()->clearFindText();
        return;
    }

    int nMatches = sourceArea()->findText(str, (searchMatchCase() ? QTextDocument::FindCaseSensitively : QTextDocument::FindFlags()));

    matchesLabel->setText(QString("(%1)").arg(nMatches));
}

void SeerEditorWidget::handleSearchDownToolButton () {

    QString str = searchTextLineEdit->text();

    if (str == "") {
        return;
    }

    sourceArea()->find(str, (searchMatchCase() ? QTextDocument::FindCaseSensitively : QTextDocument::FindFlags()));
}

void SeerEditorWidget::handleSearchUpToolButton () {

    QString str = searchTextLineEdit->text();

    if (str == "") {
        return;
    }

    sourceArea()->find(str, (searchMatchCase() ? QTextDocument::FindCaseSensitively : QTextDocument::FindFlags()) | QTextDocument::FindBackward);
}

void SeerEditorWidget::handleSearchCloseToolButton () {
    showSearchBar(false);
}

void SeerEditorWidget::handleAlternateCloseToolButton () {
    showAlternateBar(false);
}

void SeerEditorWidget::handleAlternateFileOpenToolButton () {

    //qDebug() << "fullname =" << sourceArea()->fullname();
    //qDebug() << "file     =" << sourceArea()->file();

    QString filename = QFileDialog::getOpenFileName(this, "Locate File", sourceArea()->fullname(), QString("File (%1)").arg(sourceArea()->file()), nullptr, QFileDialog::DontUseNativeDialog);

    //qDebug() << "location =" << filename;
    //qDebug() << "directory=" << QFileInfo(filename).absolutePath();

    if (filename != "") {
        alternateLineEdit->setText(QFileInfo(filename).absolutePath());
    }
}

void SeerEditorWidget::handleAlternateLineEdit () {

    showAlternateBar(false);

    QString dirname = alternateLineEdit->text();

    //qDebug() << "alternate dirname=" << dirname;

    sourceArea()->open(sourceArea()->fullname(), sourceArea()->file(), dirname);
}

void SeerEditorWidget::handleAlternateAddToGlobalToolButton () {

    if (alternateLineEdit->text() == "") {
        return;
    }

    //qDebug() << "alternate dirname=" << alternateLineEdit->text();

    emit addAlternateDirectory(alternateLineEdit->text());
}

void SeerEditorWidget::handleTextSearchShortcut () {

    if (isSearchBarShown() == true) {
        showSearchBar(false);
    }else{
        showSearchBar(true);
    }
}

void SeerEditorWidget::handleAlternateDirectoryShortcut () {

    if (isAlternateBarShown() == true) {
        showAlternateBar(false);
    }else{
        showAlternateBar(true);
    }
}

