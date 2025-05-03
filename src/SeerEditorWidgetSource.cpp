#include "SeerEditorWidgetSource.h"
#include <QtGui/QColor>
#include <QtGui/QPainter>
#include <QtGui/QTextBlock>
#include <QtGui/QFont>
#include <QtGui/QIcon>
#include <QtGui/QRadialGradient>
#include <QtWidgets/QScrollBar>
#include <QtWidgets/QFileDialog>
#include <QtWidgets/QMenu>
#include <QAction>
#include <QtGui/QTextCursor>
#include <QtGui/QPalette>
#include <QtCore/QList>
#include <QtCore/QString>
#include <QtCore/QTextStream>
#include <QtCore/QFile>
#include <QtCore/QDebug>

SeerEditorWidgetSource::SeerEditorWidgetSource(QWidget* parent) : QWidget(parent) {

    // Construct the UI.
    setupUi(this);

    // Set the widgets.
    sourceArea()->show();
    searchTextLineEdit->enableReturnPressedOnClear();

    showSearchBar(false);      // Hide the search bar. ctrl+F to show it again.
    showAlternateBar(false);   // Hide the alternate bar. ctrl+O to show it again.
    showReloadBar(false);      // Hide the reload file bar. Automatically shown if file gets updated.
    setSearchMatchCase(true);  // Search with case sensitivity.

    _textSearchShortcut       = new QShortcut(QKeySequence(tr("Ctrl+F")),       this);
    _textSearchNextShortcut   = new QShortcut(QKeySequence(tr("Ctrl+G")),       this);
    _textSearchPrevShortcut   = new QShortcut(QKeySequence(tr("Ctrl+Shift+G")), this);
    _textSearchReloadShortcut = new QShortcut(QKeySequence(tr("Ctrl+R")),       this);
    _alternateDirShortcut     = new QShortcut(QKeySequence(tr("Ctrl+O")),       this);

    setKeySettings(SeerKeySettings::populate());

    // Connect things.
    QObject::connect(searchTextLineEdit,                &QLineEdit::returnPressed,                              this,  &SeerEditorWidgetSource::handleSearchTextLineEdit);
#if QT_VERSION >= 0x060900
    QObject::connect(matchCaseCheckBox,                 &QCheckBox::checkStateChanged,                          this,  &SeerEditorWidgetSource::handleSearchTextLineEdit);
#else
    QObject::connect(matchCaseCheckBox,                 &QCheckBox::stateChanged,                               this,  &SeerEditorWidgetSource::handleSearchTextLineEdit);
#endif
    QObject::connect(searchDownToolButton,              &QToolButton::clicked,                                  this,  &SeerEditorWidgetSource::handleSearchDownToolButton);
    QObject::connect(searchUpToolButton,                &QToolButton::clicked,                                  this,  &SeerEditorWidgetSource::handleSearchUpToolButton);
    QObject::connect(searchLineNumberLineEdit,          &QLineEdit::returnPressed,                              this,  &SeerEditorWidgetSource::handleSearchLineNumberLineEdit);
    QObject::connect(searchReloadToolButton,            &QToolButton::clicked,                                  this,  &SeerEditorWidgetSource::handleReloadToolButton);
    QObject::connect(searchCloseToolButton,             &QToolButton::clicked,                                  this,  &SeerEditorWidgetSource::handleSearchCloseToolButton);
    QObject::connect(alternateCloseToolButton,          &QToolButton::clicked,                                  this,  &SeerEditorWidgetSource::handleAlternateCloseToolButton);
    QObject::connect(alternateFileOpenToolButton,       &QToolButton::clicked,                                  this,  &SeerEditorWidgetSource::handleAlternateFileOpenToolButton);
    QObject::connect(alternateLineEdit,                 &QLineEdit::returnPressed,                              this,  &SeerEditorWidgetSource::handleAlternateLineEdit);
    QObject::connect(reloadToolButton,                  &QToolButton::clicked,                                  this,  &SeerEditorWidgetSource::handleReloadToolButton);
    QObject::connect(reloadCloseToolButton,             &QToolButton::clicked,                                  this,  &SeerEditorWidgetSource::handleReloadCloseToolButton);
    QObject::connect(sourceWidget,                      &SeerEditorWidgetSourceArea::showSearchBar,             this,  &SeerEditorWidgetSource::showSearchBar);
    QObject::connect(sourceWidget,                      &SeerEditorWidgetSourceArea::showAlternateBar,          this,  &SeerEditorWidgetSource::showAlternateBar);
    QObject::connect(sourceWidget,                      &SeerEditorWidgetSourceArea::showReloadBar,             this,  &SeerEditorWidgetSource::showReloadBar);

    QObject::connect(_textSearchShortcut,               &QShortcut::activated,                                  this,  &SeerEditorWidgetSource::handleTextSearchShortcut);
    QObject::connect(_textSearchNextShortcut,           &QShortcut::activated,                                  this,  &SeerEditorWidgetSource::handleSearchDownToolButton);
    QObject::connect(_textSearchPrevShortcut,           &QShortcut::activated,                                  this,  &SeerEditorWidgetSource::handleSearchUpToolButton);
    QObject::connect(_textSearchReloadShortcut,         &QShortcut::activated,                                  this,  &SeerEditorWidgetSource::handleReloadToolButton);
    QObject::connect(_alternateDirShortcut,             &QShortcut::activated,                                  this,  &SeerEditorWidgetSource::handleAlternateDirectoryShortcut);
}

SeerEditorWidgetSource::~SeerEditorWidgetSource () {
}

SeerEditorWidgetSourceArea* SeerEditorWidgetSource::sourceArea () {

    return sourceWidget;
}

bool SeerEditorWidgetSource::isSearchBarShown () const {

    return searchBarWidget->isVisible();
}

bool SeerEditorWidgetSource::searchMatchCase () const {

    return matchCaseCheckBox->isChecked();
}

bool SeerEditorWidgetSource::isAlternateBarShown () const {

    return alternateBarWidget->isVisible();
}

void SeerEditorWidgetSource::setKeySettings (const SeerKeySettings& settings) {

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

const SeerKeySettings& SeerEditorWidgetSource::keySettings () const {

    return _keySettings;
}

void SeerEditorWidgetSource::showSearchBar (bool flag) {

    searchBarWidget->setVisible(flag);

    // If 'show', give the searchTextLineEdit the focus.
    if (flag) {
        searchTextLineEdit->setFocus(Qt::MouseFocusReason);
    }
}

void SeerEditorWidgetSource::setSearchMatchCase (bool flag) {

    matchCaseCheckBox->setChecked(flag);
}

void SeerEditorWidgetSource::showAlternateBar (bool flag) {

    alternateBarWidget->setVisible(flag);

    if (flag) {
        originalLineEdit->setText(sourceArea()->fullname());    // Set the original text.
        rememberAlternateCheckBox->setChecked(true);            // Set the default remember setting.
        alternateLineEdit->setFocus(Qt::MouseFocusReason);      // If 'show', give the alternateLineEdit the focus.
    }
}

void SeerEditorWidgetSource::showReloadBar (bool flag) {

    reloadBarWidget->setVisible(flag);
    reloadBarWidget->setStyleSheet("QWidget { background-color : #00AA00; color : #FFFF00; }");

    if (flag) {
        reloadFilenameLabel->setText("The file \"" + sourceArea()->file() + "\" has changed on disk.");
        reloadToolButton->setFocus(Qt::MouseFocusReason);      // If 'show', give the reload button the focus.
    }else{
        reloadFilenameLabel->setText("");
    }
}

void SeerEditorWidgetSource::handleSearchLineNumberLineEdit () {

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

void SeerEditorWidgetSource::handleSearchTextLineEdit () {

    QString str = searchTextLineEdit->text();

    matchesLabel->setText("");

    if (str == "") {
        sourceArea()->clearFindText();
        return;
    }

    int nMatches = sourceArea()->findText(str, (searchMatchCase() ? QTextDocument::FindCaseSensitively : QTextDocument::FindFlags()));

    matchesLabel->setText(QString("(%1)").arg(nMatches));
}

void SeerEditorWidgetSource::handleSearchDownToolButton () {

    QString str = searchTextLineEdit->text();

    if (str == "") {
        return;
    }

    sourceArea()->find(str, (searchMatchCase() ? QTextDocument::FindCaseSensitively : QTextDocument::FindFlags()));
}

void SeerEditorWidgetSource::handleSearchUpToolButton () {

    QString str = searchTextLineEdit->text();

    if (str == "") {
        return;
    }

    sourceArea()->find(str, (searchMatchCase() ? QTextDocument::FindCaseSensitively : QTextDocument::FindFlags()) | QTextDocument::FindBackward);
}

void SeerEditorWidgetSource::handleSearchCloseToolButton () {
    showSearchBar(false);
}

void SeerEditorWidgetSource::handleAlternateCloseToolButton () {
    showAlternateBar(false);
}

void SeerEditorWidgetSource::handleAlternateFileOpenToolButton () {

    //qDebug() << "fullname =" << sourceArea()->fullname();
    //qDebug() << "file     =" << sourceArea()->file();

    QString filename = QFileDialog::getOpenFileName(this, "Locate File", sourceArea()->fullname(), QString("File (%1)").arg(sourceArea()->file()), nullptr, QFileDialog::DontUseNativeDialog);

    //qDebug() << "location =" << filename;
    //qDebug() << "directory=" << QFileInfo(filename).absolutePath();

    if (filename != "") {
        alternateLineEdit->setText(QFileInfo(filename).absolutePath());
    }

    handleAlternateLineEdit();
}

void SeerEditorWidgetSource::handleAlternateLineEdit () {

    showAlternateBar(false);

    // Get the base path.
    QString dirname = alternateLineEdit->text();

    if (dirname == "") {
        return;
    }

    //qDebug() << "alternate dirname=" << dirname;

    // Attempt to open the file.
    sourceArea()->open(sourceArea()->fullname(), sourceArea()->file(), dirname);

    // Add to global alternate list.
    if (rememberAlternateCheckBox->isChecked()) {
        emit addAlternateDirectory(dirname);
    }
}

void SeerEditorWidgetSource::handleTextSearchShortcut () {

    if (isSearchBarShown() == true) {
        showSearchBar(false);
    }else{
        showSearchBar(true);
    }
}

void SeerEditorWidgetSource::handleAlternateDirectoryShortcut () {

    if (isAlternateBarShown() == true) {
        showAlternateBar(false);
    }else{
        showAlternateBar(true);
    }
}

void SeerEditorWidgetSource::handleReloadToolButton () {

    showReloadBar(false);

    sourceArea()->reload();
}

void SeerEditorWidgetSource::handleReloadCloseToolButton () {

    showReloadBar(false);
}

