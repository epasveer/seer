#include "SeerEditorWidgetAssembly.h"
#include "SeerAssemblyPreferenceDialog.h"
#include "SeerUtl.h"
#include <QtGui/QColor>
#include <QtGui/QPainter>
#include <QtGui/QTextBlock>
#include <QtGui/QFont>
#include <QtGui/QIcon>
#include <QtGui/QAction>
#include <QtGui/QTextCursor>
#include <QtGui/QPalette>
#include <QtWidgets/QScrollBar>
#include <QtWidgets/QFileDialog>
#include <QtWidgets/QMenu>
#include <QtCore/QSettings>
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

    _pcId      = Seer::createID();
    _spId      = Seer::createID();
    _flagsId   = Seer::createID();

    // Clear PC, SP, and FLAGS.
    pcLineEdit->clear();
    spLineEdit->clear();
    flagsLineEdit->clear();

    // Preference menu.
    QMenu* menu = new QMenu();

    _editPreferencesAction = menu->addAction("Assembly preferences");

    preferencesToolButton->setMenu(menu);
    preferencesToolButton->setPopupMode(QToolButton::InstantPopup);

    // Connect things.
    QObject::connect(searchTextLineEdit,                &QLineEdit::returnPressed,                      this,  &SeerEditorWidgetAssembly::handleSearchTextLineEdit);
#if QT_VERSION >= 0x060900
    QObject::connect(matchCaseCheckBox,                 &QCheckBox::checkStateChanged,                  this,  &SeerEditorWidgetAssembly::handleSearchTextLineEdit);
#else
    QObject::connect(matchCaseCheckBox,                 &QCheckBox::stateChanged,                       this,  &SeerEditorWidgetAssembly::handleSearchTextLineEdit);
#endif
    QObject::connect(searchDownToolButton,              &QToolButton::clicked,                          this,  &SeerEditorWidgetAssembly::handleSearchDownToolButton);
    QObject::connect(searchUpToolButton,                &QToolButton::clicked,                          this,  &SeerEditorWidgetAssembly::handleSearchUpToolButton);
    QObject::connect(searchLineNumberLineEdit,          &QLineEdit::returnPressed,                      this,  &SeerEditorWidgetAssembly::handleSearchLineNumberLineEdit);
    QObject::connect(searchCloseToolButton,             &QToolButton::clicked,                          this,  &SeerEditorWidgetAssembly::handleSearchCloseToolButton);
    QObject::connect(refreshToolButton,                 &QToolButton::clicked,                          this,  &SeerEditorWidgetAssembly::reloadAssembly);
    QObject::connect(refreshToolButton,                 &QToolButton::clicked,                          this,  &SeerEditorWidgetAssembly::reloadRegisters);
#if QT_VERSION >= 0x060900
    QObject::connect(showAddressCheckBox,               &QCheckBox::checkStateChanged,                  this,  &SeerEditorWidgetAssembly::handleShowAddressColumn);
    QObject::connect(showOffsetCheckBox,                &QCheckBox::checkStateChanged,                  this,  &SeerEditorWidgetAssembly::handleShowOffsetColumn);
    QObject::connect(showOpcodeCheckBox,                &QCheckBox::checkStateChanged,                  this,  &SeerEditorWidgetAssembly::handleShowOpcodeColumn);
    QObject::connect(showSourceCheckBox,                &QCheckBox::checkStateChanged,                  this,  &SeerEditorWidgetAssembly::handleShowSourceLines);
#else
    QObject::connect(showAddressCheckBox,               &QCheckBox::stateChanged,                       this,  &SeerEditorWidgetAssembly::handleShowAddressColumn);
    QObject::connect(showOffsetCheckBox,                &QCheckBox::stateChanged,                       this,  &SeerEditorWidgetAssembly::handleShowOffsetColumn);
    QObject::connect(showOpcodeCheckBox,                &QCheckBox::stateChanged,                       this,  &SeerEditorWidgetAssembly::handleShowOpcodeColumn);
    QObject::connect(showSourceCheckBox,                &QCheckBox::stateChanged,                       this,  &SeerEditorWidgetAssembly::handleShowSourceLines);
#endif
    QObject::connect(assemblyWidget,                    &SeerEditorWidgetAssemblyArea::showSearchBar,   this,  &SeerEditorWidgetAssembly::showSearchBar);

    QObject::connect(_textSearchShortcut,               &QShortcut::activated,                          this,  &SeerEditorWidgetAssembly::handleTextSearchShortcut);
    QObject::connect(_textSearchNextShortcut,           &QShortcut::activated,                          this,  &SeerEditorWidgetAssembly::handleSearchDownToolButton);
    QObject::connect(_textSearchPrevShortcut,           &QShortcut::activated,                          this,  &SeerEditorWidgetAssembly::handleSearchUpToolButton);

    QObject::connect(_editPreferencesAction,            &QAction::triggered,                            this,  &SeerEditorWidgetAssembly::handleEditPreferences);

    // Set defaults.
    setShowAddressColumn(true);
    setShowOffsetColumn(false);
    setShowOpcodeColumn(false);
    setShowSourceLines(false);

    setRegiserNamePC("$pc");
    setRegiserNameSP("$sp");
    setRegiserNameFLAGS("$ps");

    readSettings();
}

SeerEditorWidgetAssembly::~SeerEditorWidgetAssembly () {
}

SeerEditorWidgetAssemblyArea* SeerEditorWidgetAssembly::assemblyArea () {

    return assemblyWidget;
}

bool SeerEditorWidgetAssembly::isSearchBarShown () const {

    return searchBarWidget->isVisible();
}

bool SeerEditorWidgetAssembly::searchMatchCase () const {

    return matchCaseCheckBox->isChecked();
}

bool SeerEditorWidgetAssembly::showAddressColumn () const {

    return showAddressCheckBox->isChecked();
}

bool SeerEditorWidgetAssembly::showOffsetColumn () const {

    return showOffsetCheckBox->isChecked();
}

bool SeerEditorWidgetAssembly::showOpcodeColumn () const {

    return showOpcodeCheckBox->isChecked();
}

bool SeerEditorWidgetAssembly::showSourceLines () const {

    return showSourceCheckBox->isChecked();
}

QString SeerEditorWidgetAssembly::regiserNamePC () const {

    return _pcName;
}

QString SeerEditorWidgetAssembly::regiserNameFLAGS () const {

    return _flagsName;
}

QString SeerEditorWidgetAssembly::regiserNameSP () const {

    return _spName;
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

void SeerEditorWidgetAssembly::reloadAssembly () {

    QString addr = assemblyArea()->address();

    assemblyArea()->setAddress(addr, true);

    pcLineEdit->clear();
    spLineEdit->clear();
    flagsLineEdit->clear();

    // Get the PC, SP, and FLAGS
    if (regiserNamePC() != "") {
        emit evaluateVariableExpression(_pcId, regiserNamePC());
    }
    if (regiserNameSP() != "") {
        emit evaluateVariableExpression(_spId, regiserNameSP());
    }
    if (regiserNameFLAGS() != "") {
        emit evaluateVariableExpression(_flagsId, regiserNameFLAGS());
    }
}

void SeerEditorWidgetAssembly::reloadRegisters () {

    pcLineEdit->clear();
    spLineEdit->clear();
    flagsLineEdit->clear();

    // Get the PC, SP, and FLAGS
    if (regiserNamePC() != "") {
        emit evaluateVariableExpression(_pcId, regiserNamePC());
    }
    if (regiserNameSP() != "") {
        emit evaluateVariableExpression(_spId, regiserNameSP());
    }
    if (regiserNameFLAGS() != "") {
        emit evaluateVariableExpression(_flagsId, regiserNameFLAGS());
    }
}

void SeerEditorWidgetAssembly::showSearchBar (bool flag) {

    searchBarWidget->setVisible(flag);

    // If 'show', give the searchTextLineEdit the focus.
    if (flag) {
        searchTextLineEdit->setFocus(Qt::MouseFocusReason);
    }
}

void SeerEditorWidgetAssembly::setSearchMatchCase (bool flag) {

    matchCaseCheckBox->setChecked(flag);
}

void SeerEditorWidgetAssembly::setShowAddressColumn (bool flag) {

    showAddressCheckBox->setChecked(flag);

    handleShowAddressColumn();
}

void SeerEditorWidgetAssembly::setShowOffsetColumn (bool flag) {

    showOffsetCheckBox->setChecked(flag);

    handleShowOffsetColumn();
}

void SeerEditorWidgetAssembly::setShowOpcodeColumn (bool flag) {

    showOpcodeCheckBox->setChecked(flag);

    handleShowOpcodeColumn();
}

void SeerEditorWidgetAssembly::setShowSourceLines (bool flag) {

    showSourceCheckBox->setChecked(flag);

    handleShowSourceLines();
}

void SeerEditorWidgetAssembly::setRegiserNamePC (const QString& name) {

    _pcName = name;
}

void SeerEditorWidgetAssembly::setRegiserNameFLAGS (const QString& name) {

    _flagsName = name;
}

void SeerEditorWidgetAssembly::setRegiserNameSP (const QString& name) {

    _spName = name;
}

void SeerEditorWidgetAssembly::handleText (const QString& text) {

    if (text.startsWith("*stopped")) {

        //qDebug() << text;

        // *stopped,
        //
        // reason="end-stepping-range",
        //
        // frame={addr="0x0000000000400b45",
        //        func="main",
        //        args=[{name="argc",value="1"},{name="argv",value="0x7fffffffd5b8"}],
        //        file="helloworld.cpp",
        //        fullname="/home/erniep/Development/Peak/src/Seer/helloworld/helloworld.cpp",
        //        line="7",
        //        arch="i386:x86-64"},
        //
        // thread-id="1",
        // stopped-threads="all",
        // core="6"

        QString newtext = Seer::filterEscapes(text); // Filter escaped characters.

        QString frame_text = Seer::parseFirst(newtext, "frame=", '{', '}', false);

        if (frame_text == "") {
            return;
        }

        // Get the PC, SP, and FLAGS
        if (regiserNamePC() != "") {
            emit evaluateVariableExpression(_pcId, regiserNamePC());
        }
        if (regiserNameSP() != "") {
            emit evaluateVariableExpression(_spId, regiserNameSP());
        }
        if (regiserNameFLAGS() != "") {
            emit evaluateVariableExpression(_flagsId, regiserNameFLAGS());
        }

        return;

    }else if (text.contains(QRegularExpression("^([0-9]+)\\^done,value="))) {

        QString id_text    = text.section('^', 0,0);
        QString value_text = Seer::parseFirst(text, "value=", '"', '"', false);

        if (id_text == QString::number(_pcId)) {
            pcLineEdit->setText(Seer::filterEscapes(value_text));
            return;
        }

        if (id_text == QString::number(_spId)) {
            spLineEdit->setText(Seer::filterEscapes(value_text));
            return;
        }

        if (id_text == QString::number(_flagsId)) {
            flagsLineEdit->setText(Seer::filterEscapes(value_text));
            return;
        }

    }else if (text.contains(QRegularExpression("^([0-9]+)\\^error,msg="))) {

        QString id_text  = text.section('^', 0,0);
        QString msg_text = Seer::parseFirst(text, "value=", '"', '"', false);

        if (id_text == QString::number(_pcId)) {
            pcLineEdit->setText(Seer::filterEscapes(msg_text));
            return;
        }

        if (id_text == QString::number(_spId)) {
            spLineEdit->setText(Seer::filterEscapes(msg_text));
            return;
        }

        if (id_text == QString::number(_flagsId)) {
            flagsLineEdit->setText(Seer::filterEscapes(msg_text));
            return;
        }

    }else if (text.startsWith("^error,msg=\"No registers.\"")) {

        // Clear PC, SP, and FLAGS.
        pcLineEdit->clear();
        spLineEdit->clear();
        flagsLineEdit->clear();

    }else{
        // Ignore others.
    }
}

void SeerEditorWidgetAssembly::handleSearchLineNumberLineEdit () {

    QString address = searchLineNumberLineEdit->text();

    if (address == "") {
        return;
    }

    searchLineNumberLineEdit->clear();

    // Will emit the load signal, if 'address' is not already loaded.
    assemblyArea()->setAddress(address);
    assemblyArea()->setCurrentLine(address);
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

void SeerEditorWidgetAssembly::handleShowAddressColumn () {

    assemblyArea()->enableLineNumberArea(showAddressColumn());
}

void SeerEditorWidgetAssembly::handleShowOffsetColumn () {

    assemblyArea()->enableOffsetArea(showOffsetColumn());
}

void SeerEditorWidgetAssembly::handleShowOpcodeColumn () {

    assemblyArea()->enableOpcodeArea(showOpcodeColumn());
}

void SeerEditorWidgetAssembly::handleShowSourceLines () {

    assemblyArea()->enableSourceLines(showSourceLines());
}

void SeerEditorWidgetAssembly::handleEditPreferences () {

    // Bring up the register profile dialog.
    SeerAssemblyPreferenceDialog dlg(this);

    dlg.setRegiserNamePC(regiserNamePC());
    dlg.setRegiserNameFLAGS(regiserNameFLAGS());
    dlg.setRegiserNameSP(regiserNameSP());
    dlg.setShowAssemblyAddress(showAddressColumn());
    dlg.setShowAssemblyOffset(showOffsetColumn());
    dlg.setShowAssemblyOpcode(showOpcodeColumn());
    dlg.setShowAssemblySource(showSourceLines());

    if (dlg.exec()) {

        setRegiserNamePC(dlg.regiserNamePC());
        setRegiserNameFLAGS(dlg.regiserNameFLAGS());
        setRegiserNameSP(dlg.regiserNameSP());
        setShowAddressColumn(dlg.showAssemblyAddress());
        setShowOffsetColumn(dlg.showAssemblyOffset());
        setShowOpcodeColumn(dlg.showAssemblyOpcode());
        setShowSourceLines(dlg.showAssemblySource());

        reloadRegisters();

        writeSettings();
    }
}

void SeerEditorWidgetAssembly::writeSettings () {

    QSettings settings;

    settings.beginGroup("assembly"); {
        settings.setValue("assemblyregisternamepc",      regiserNamePC());
        settings.setValue("assemblyregisternameflags",   regiserNameFLAGS());
        settings.setValue("assemblyregisternamesp",      regiserNameSP());
        settings.setValue("assemblyshowaddresscolumn",   showAddressColumn());
        settings.setValue("assemblyshowoffsetcolumn",    showOffsetColumn());
        settings.setValue("assemblyshowopcodecolumn",    showOpcodeColumn());
        settings.setValue("assemblyshowsourcelines",     showSourceLines());
    } settings.endGroup();
}

void SeerEditorWidgetAssembly::readSettings () {

    QSettings settings;

    settings.beginGroup("assembly"); {
        setRegiserNamePC     (settings.value("assemblyregisternamepc",    "$pc").toString());
        setRegiserNameFLAGS  (settings.value("assemblyregisternameflags", "$ps").toString());
        setRegiserNameSP     (settings.value("assemblyregisternamesp",    "$sp").toString());
        setShowAddressColumn (settings.value("assemblyshowaddresscolumn", true).toBool());
        setShowOffsetColumn  (settings.value("assemblyshowoffsetcolumn",  false).toBool());
        setShowOpcodeColumn  (settings.value("assemblyshowopcodecolumn",  false).toBool());
        setShowSourceLines   (settings.value("assemblyshowsourcelines",   false).toBool());
    } settings.endGroup();
}

