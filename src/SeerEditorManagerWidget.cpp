#include "SeerEditorManagerWidget.h"
#include "SeerEditorWidgetSource.h"
#include "SeerEditorWidgetAssembly.h"
#include "SeerCloseSourceDialog.h"
#include "SeerHelpPageDialog.h"
#include "SeerUtl.h"
#include "QHContainerWidget.h"
#include <QtWidgets/QToolButton>
#include <QtWidgets/QFileDialog>
#include <QtWidgets/QMessageBox>
#include <QtCore/QString>
#include <QtCore/QTextStream>
#include <QtCore/QFile>
#include <QtCore/QFileInfo>
#include <QtCore/QDebug>

SeerEditorManagerWidget::SeerEditorManagerWidget (QWidget* parent) : QWidget(parent) {

    // Initialize private data
    _editorFont                     = QFont("monospace", 10);                      // Default font.
    _editorHighlighterSettings      = SeerHighlighterSettings::populateForCPP(""); // Default syntax highlighting.
    _editorHighlighterEnabled       = true;
    _editorKeySettings              = SeerKeySettings::populate();                 // Default key settings.
    _editorTabSize                  = 4;
    _editorExternalEditorCommand    = "";
    _assemblyWidget                 = 0;
    _keepAssemblyTabOnTop           = true;
    _showAddressColumn              = true;
    _showOffsetColumn               = false;
    _showOpcodeColumn               = false;
    _showSourceLines                = false;
    _notifyAssemblyTabShown         = true;

    // Setup UI
    setupUi(this);

    // Setup the widgets
    tabWidget->setMovable(true);
    tabWidget->setTabsClosable(true);

    // Create editor options bar.
    QToolButton* fileOpenToolButton = new QToolButton(tabWidget);
    fileOpenToolButton->setIcon(QIcon(":/seer/resources/RelaxLightIcons/document-open.svg"));
    fileOpenToolButton->setToolTip("Open a file.");

    QToolButton* fileCloseToolButton = new QToolButton(tabWidget);
    fileCloseToolButton->setIcon(QIcon(":/seer/resources/RelaxLightIcons/list-remove.svg"));
    fileCloseToolButton->setToolTip("Close opened files.");

    QToolButton* textSearchToolButton = new QToolButton(tabWidget);
    textSearchToolButton->setIcon(QIcon(":/seer/resources/RelaxLightIcons/edit-find.svg"));
    textSearchToolButton->setToolTip("Show search bar.");

    QToolButton* helpToolButton = new QToolButton(tabWidget);
    helpToolButton->setIcon(QIcon(":/seer/resources/RelaxLightIcons/help-about.svg"));
    helpToolButton->setToolTip("Help on the code manager.");

    QHContainerWidget* hcontainer = new QHContainerWidget(this);
    hcontainer->setSpacing(3);
    hcontainer->addWidget(fileOpenToolButton);
    hcontainer->addWidget(fileCloseToolButton);
    hcontainer->addWidget(textSearchToolButton);
    hcontainer->addWidget(helpToolButton);

    tabWidget->setCornerWidget(hcontainer, Qt::TopRightCorner);

    // Create a place holder tab with a special name of "".
    createEditorWidgetTab("", "");

    // Connect things.
    QObject::connect(tabWidget,             &QTabWidget::tabCloseRequested,    this, &SeerEditorManagerWidget::handleTabCloseRequested);
    QObject::connect(tabWidget,             &QTabWidget::currentChanged,       this, &SeerEditorManagerWidget::handleTabCurrentChanged);
    QObject::connect(fileOpenToolButton,    &QToolButton::clicked,             this, &SeerEditorManagerWidget::handleFileOpenToolButtonClicked);
    QObject::connect(fileCloseToolButton,   &QToolButton::clicked,             this, &SeerEditorManagerWidget::handleFileCloseToolButtonClicked);
    QObject::connect(textSearchToolButton,  &QToolButton::clicked,             this, &SeerEditorManagerWidget::handleTextSearchToolButtonClicked);
    QObject::connect(helpToolButton,        &QToolButton::clicked,             this, &SeerEditorManagerWidget::handleHelpToolButtonClicked);
}

SeerEditorManagerWidget::~SeerEditorManagerWidget () {

    _notifyAssemblyTabShown = false;

    deleteAssemblyWidgetTab();
}

void SeerEditorManagerWidget::dumpEntries () const {

    qDebug();

    SeerEditorManagerEntries::const_iterator b = beginEntry();
    SeerEditorManagerEntries::const_iterator e = endEntry();

    qDebug() << "Start";
    while (b != e) {
        qDebug() << "\tFullname:" << b->fullname << "File:" << b->file;
        b++;
    }
    qDebug() << "End";
}

bool SeerEditorManagerWidget::hasEntry (const QString& fullname) const {

    if (_entries.find(fullname) != _entries.end()) {
        return true;
    }

    return false;
}

SeerEditorManagerEntries::iterator SeerEditorManagerWidget::addEntry (const QString& fullname, const QString& file) {

    //qDebug() << "Add entry:" << fullname << file;

    SeerEditorManagerEntry entry;

    entry.fullname = fullname;
    entry.file     = file;
    entry.widget   = 0;

    return _entries.insert(fullname, entry);
}

SeerEditorManagerEntries::iterator SeerEditorManagerWidget::findEntry (const QString& fullname) {
    return _entries.find(fullname);
}

SeerEditorManagerEntries::const_iterator SeerEditorManagerWidget::findEntry (const QString& fullname) const {
    return _entries.find(fullname);
}

SeerEditorManagerEntries::iterator SeerEditorManagerWidget::beginEntry () {
    return _entries.begin();
}

SeerEditorManagerEntries::const_iterator SeerEditorManagerWidget::beginEntry () const {
    return _entries.begin();
}

SeerEditorManagerEntries::iterator SeerEditorManagerWidget::endEntry () {
    return _entries.end();
}

SeerEditorManagerEntries::const_iterator SeerEditorManagerWidget::endEntry () const {
    return _entries.end();
}

void SeerEditorManagerWidget::deleteEntry (SeerEditorManagerEntries::iterator i) {
    _entries.erase(i);
}

void SeerEditorManagerWidget::showAssembly () {

    // Create and show the assembly widget if it isn't already.
    if (assemblyWidgetTab() == 0) {
        createAssemblyWidgetTab();
    }

    assemblyWidgetTab()->assemblyArea()->setAddress("$pc");
    assemblyWidgetTab()->reloadRegisters();
}

SeerEditorWidgetAssembly* SeerEditorManagerWidget::assemblyWidgetTab () {

    return _assemblyWidget;
}

void SeerEditorManagerWidget::setKeepAssemblyTabOnTop (bool flag) {

    _keepAssemblyTabOnTop = flag;
}

bool SeerEditorManagerWidget::keepAssemblyTabOnTop () const {

    return _keepAssemblyTabOnTop;
}

void SeerEditorManagerWidget::setAssemblyShowAddressColumn (bool flag) {

    _showAddressColumn = flag;

    if (assemblyWidgetTab() != 0) {
        assemblyWidgetTab()->setShowAddressColumn(_showAddressColumn);
    }
}

bool SeerEditorManagerWidget::assemblyShowAddressColumn () const {

    return _showAddressColumn;
}

void SeerEditorManagerWidget::setAssemblyShowOffsetColumn (bool flag) {

    _showOffsetColumn = flag;

    if (assemblyWidgetTab() != 0) {
        assemblyWidgetTab()->setShowOffsetColumn(_showOffsetColumn);
    }
}

bool SeerEditorManagerWidget::assemblyShowOffsetColumn () const {

    return _showOffsetColumn;
}

void SeerEditorManagerWidget::setAssemblyShowOpcodeColumn (bool flag) {

    _showOpcodeColumn = flag;

    if (assemblyWidgetTab() != 0) {
        assemblyWidgetTab()->setShowOpcodeColumn(_showOpcodeColumn);
    }
}

bool SeerEditorManagerWidget::assemblyShowOpcodeColumn () const {

    return _showOpcodeColumn;
}

void SeerEditorManagerWidget::setAssemblyShowSourceLines (bool flag) {

    _showSourceLines = flag;

    if (assemblyWidgetTab() != 0) {
        assemblyWidgetTab()->setShowSourceLines(_showSourceLines);
    }
}

bool SeerEditorManagerWidget::assemblyShowSourceLines () const {

    return _showSourceLines;
}

SeerEditorManagerFiles SeerEditorManagerWidget::openedFiles () const {

    SeerEditorManagerFiles files;

    SeerEditorManagerEntries::const_iterator b = beginEntry();
    SeerEditorManagerEntries::const_iterator e = endEntry();

    while (b != e) {

        SeerEditorManagerFile f;

        f.file     = b->file;
        f.fullname = b->fullname;

        files.push_back(f);

        b++;
    }

    return files;
}

void SeerEditorManagerWidget::setEditorFont (const QFont& font) {

    _editorFont = font;

    // Update current editors.
    SeerEditorManagerEntries::iterator b = beginEntry();
    SeerEditorManagerEntries::iterator e = endEntry();

    while (b != e) {
        b->widget->sourceArea()->setEditorFont(editorFont());
        b++;
    }

    // Don't forget about the assembly widget.
    SeerEditorWidgetAssembly* assemblyWidget = assemblyWidgetTab();

    if (assemblyWidget) {
        assemblyWidget->assemblyArea()->setEditorFont(editorFont());
    }
}

const QFont& SeerEditorManagerWidget::editorFont () const {

    return _editorFont;
}

void SeerEditorManagerWidget::setEditorHighlighterSettings (const SeerHighlighterSettings& settings) {

    _editorHighlighterSettings = settings;

    // Update current editors.
    SeerEditorManagerEntries::iterator b = beginEntry();
    SeerEditorManagerEntries::iterator e = endEntry();

    while (b != e) {
        b->widget->sourceArea()->setHighlighterSettings(_editorHighlighterSettings);
        b++;
    }

    // Don't forget about the assembly widget.
    SeerEditorWidgetAssembly* assemblyWidget = assemblyWidgetTab();

    if (assemblyWidget) {
        assemblyWidget->assemblyArea()->setHighlighterSettings(editorHighlighterSettings());
    }
}

const SeerHighlighterSettings& SeerEditorManagerWidget::editorHighlighterSettings () const {

    return _editorHighlighterSettings;
}

void SeerEditorManagerWidget::setEditorHighlighterEnabled (bool flag) {

    _editorHighlighterEnabled = flag;

    // Update current editors.
    SeerEditorManagerEntries::iterator b = beginEntry();
    SeerEditorManagerEntries::iterator e = endEntry();

    while (b != e) {
        b->widget->sourceArea()->setHighlighterEnabled(_editorHighlighterEnabled);
        b++;
    }

    // Don't forget about the assembly widget.
    SeerEditorWidgetAssembly* assemblyWidget = assemblyWidgetTab();

    if (assemblyWidget) {
        assemblyWidget->assemblyArea()->setHighlighterEnabled(editorHighlighterEnabled());
    }
}

bool SeerEditorManagerWidget::editorHighlighterEnabled () const {

    return _editorHighlighterEnabled;
}

void SeerEditorManagerWidget::setEditorAlternateDirectories (const QStringList alternateDirectories) {

    _editorAlternateDirectories = alternateDirectories;

    // Update current editors.
    SeerEditorManagerEntries::iterator b = beginEntry();
    SeerEditorManagerEntries::iterator e = endEntry();

    while (b != e) {
        b->widget->sourceArea()->setAlternateDirectories(_editorAlternateDirectories);
        b++;
    }
}

const QStringList& SeerEditorManagerWidget::editorAlternateDirectories () const {

    return _editorAlternateDirectories;
}

void SeerEditorManagerWidget::setEditorIgnoreDirectories (const QStringList ignoreDirectories) {

    _editorIgnoreDirectories = ignoreDirectories;
}

const QStringList& SeerEditorManagerWidget::editorIgnoreDirectories () const {

    return _editorIgnoreDirectories;
}

void SeerEditorManagerWidget::setEditorKeySettings (const SeerKeySettings& settings) {

    _editorKeySettings = settings;

    // Update current editors.
    SeerEditorManagerEntries::iterator b = beginEntry();
    SeerEditorManagerEntries::iterator e = endEntry();

    while (b != e) {
        b->widget->setKeySettings(_editorKeySettings);
        b++;
    }

    // Don't forget about the assembly widget.
    SeerEditorWidgetAssembly* assemblyWidget = assemblyWidgetTab();

    if (assemblyWidget) {
        assemblyWidget->setKeySettings(editorKeySettings());
    }
}

const SeerKeySettings& SeerEditorManagerWidget::editorKeySettings () const {

    return _editorKeySettings;
}

void SeerEditorManagerWidget::setEditorTabSize (int spaces) {

    _editorTabSize = spaces;
}

int SeerEditorManagerWidget::editorTabSize () const {

    return _editorTabSize;
}

void SeerEditorManagerWidget::setEditorExternalEditorCommand (const QString& externalEditorCommand) {

    _editorExternalEditorCommand = externalEditorCommand;

    // Update current editors.
    SeerEditorManagerEntries::iterator b = beginEntry();
    SeerEditorManagerEntries::iterator e = endEntry();

    while (b != e) {
        b->widget->sourceArea()->setExternalEditorCommand(_editorExternalEditorCommand);
        b++;
    }
}

const QString& SeerEditorManagerWidget::editorExternalEditorCommand () const {

    return _editorExternalEditorCommand;
}

void SeerEditorManagerWidget::handleText (const QString& text) {

    if (text.startsWith("*stopped")) {

        //qDebug() << ":stopped:" << text;

        QString newtext = Seer::filterEscapes(text); // Filter escaped characters.

        QString frame_text = Seer::parseFirst(newtext, "frame=", '{', '}', false);

        if (frame_text == "") {
            return;
        }

        QString fullname_text = Seer::parseFirst(frame_text, "fullname=", '"', '"', false);
        QString file_text     = Seer::parseFirst(frame_text, "file=",     '"', '"', false);
        QString line_text     = Seer::parseFirst(frame_text, "line=",     '"', '"', false);

        //qDebug() << frame_text;
        //qDebug() << fullname_text << file_text << line_text;

        // If there is a file to open, open it.
        if (fullname_text != "" && file_text != "") {

            // Get the EditorWidget for the file. Create one if needed.
            SeerEditorWidgetSource* editorWidget = editorWidgetTab(fullname_text);

            if (editorWidget == 0) {
                editorWidget = createEditorWidgetTab(fullname_text, file_text, text);
            }

            // Can still be null, if the file is ignored.
            if (editorWidget == 0) {
                return;
            }

            // Push this tab to the top only if the current one in not the "Assembly" tab.
            QString tabtext = "";

            if (tabWidget->currentIndex() >= 0) {
                tabtext = tabWidget->tabText(tabWidget->currentIndex());
            }

            if (keepAssemblyTabOnTop() && tabtext == "Assembly") {
                // Do nothing. The "Assembly" tab is already on top.
            }else{
                tabWidget->setCurrentWidget(editorWidget);
            }

            // Give the EditorWidget the command text (read file, set line number, etc.).
            editorWidget->sourceArea()->handleText(text);
        }

        // Get the AssemblyWidget, if there is one.
        SeerEditorWidgetAssembly* assemblyWidget = assemblyWidgetTab();

        if (assemblyWidget) {
            assemblyWidget->assemblyArea()->handleText(text);
            assemblyWidget->handleText(text);
        }

        // Handle certain reasons uniquely.
        QString reason_text = Seer::parseFirst(newtext, "reason=", '"', '"', false);

        if (reason_text == "breakpoint-hit") {
            QString disp_text = Seer::parseFirst(newtext, "disp=", '"', '"', false);

            // Ask for the breakpoint list to be resent, in case the encountered breakpoint was temporary.
            if (disp_text == "del") {
                emit refreshBreakpointsList();
            }
        }

        return;

    }else if (text.startsWith("^done,BreakpointTable={") && text.endsWith("}")) {

        //
        // See SeerBreakpointsBrowserWidget.cpp
        //
        // ^done,BreakpointTable={
        //    ...
        // }
        //

        // We have a breakpoint table. Start by clearing all breakpoints
        // in the editor widgets that are opened.
        SeerEditorManagerEntries::iterator b = beginEntry();
        SeerEditorManagerEntries::iterator e = endEntry();

        while (b != e) {
            b->widget->sourceArea()->clearBreakpoints();
            b++;
        }

        // Tell the assembly widget to clear its breakpoints.
        SeerEditorWidgetAssembly* assemblyWidget = assemblyWidgetTab();

        if (assemblyWidget) {
            assemblyWidget->assemblyArea()->clearBreakpoints();
        }

        // Now parse the table and re-add the breakpoints.
        QString newtext = Seer::filterEscapes(text); // Filter escaped characters.

        QString body_text = Seer::parseFirst(newtext, "body=", '[', ']', false);

        //qDebug() << body_text;

        if (body_text != "") {

            QStringList bkpt_list = Seer::parse(newtext, "bkpt=", '{', '}', false);

            for ( const auto& bkpt_text : bkpt_list  ) {
                QString number_text            = Seer::parseFirst(bkpt_text, "number=",            '"', '"', false);
                QString type_text              = Seer::parseFirst(bkpt_text, "type=",              '"', '"', false);
                QString disp_text              = Seer::parseFirst(bkpt_text, "disp=",              '"', '"', false);
                QString enabled_text           = Seer::parseFirst(bkpt_text, "enabled=",           '"', '"', false);
                QString addr_text              = Seer::parseFirst(bkpt_text, "addr=",              '"', '"', false);
                QString func_text              = Seer::parseFirst(bkpt_text, "func=",              '"', '"', false);
                QString file_text              = Seer::parseFirst(bkpt_text, "file=",              '"', '"', false);
                QString fullname_text          = Seer::parseFirst(bkpt_text, "fullname=",          '"', '"', false);
                QString line_text              = Seer::parseFirst(bkpt_text, "line=",              '"', '"', false);
                QString thread_groups_text     = Seer::parseFirst(bkpt_text, "thread-groups=",     '[', ']', false);
                QString times_text             = Seer::parseFirst(bkpt_text, "times=",             '"', '"', false);
                QString original_location_text = Seer::parseFirst(bkpt_text, "original-location=", '"', '"', false);

                // Find the appropriate source file and update its breakpoints
                SeerEditorManagerEntries::iterator i = findEntry(fullname_text);
                SeerEditorManagerEntries::iterator e = endEntry();

                if (i != e) {
                    i->widget->sourceArea()->addBreakpoint(number_text.toInt(), line_text.toInt(), (enabled_text == "y" ? true : false));
                }

                // Tell the assembly widget about the breakpoint. If the address is in its range, it will add it.
                SeerEditorWidgetAssembly* assemblyWidget = assemblyWidgetTab();

                if (assemblyWidget) {
                    assemblyWidget->assemblyArea()->addBreakpoint(number_text.toInt(), addr_text, (enabled_text == "y" ? true : false));
                }
            }
        }

    }else if (text.startsWith("^done,stack=[") && text.endsWith("]")) {

        //qDebug() << ":stack:" << text;

        //
        // See SeerStackFramesBrowserWidget.cpp
        // ^done,stack=[
        //     ...
        // ]
        //

        // Now parse the table and re-add the breakpoints.
        QString newtext = Seer::filterEscapes(text); // Filter escaped characters.

        QString stack_text = Seer::parseFirst(newtext, "stack=", '[', ']', false);

        if (stack_text != "") {

            // Clear current lines in all opened editor widgets.
            SeerEditorManagerEntries::iterator b = beginEntry();
            SeerEditorManagerEntries::iterator e = endEntry();

            while (b != e) {
              //b->widget->setCurrentLine(0);
                b->widget->sourceArea()->clearCurrentLines();
                b++;
            }

            // Parse through the frame list and set the current lines that are in the frame list.
            QStringList frame_list = Seer::parse(newtext, "frame=", '{', '}', false);

            for ( const auto& frame_text : frame_list  ) {
                QString level_text    = Seer::parseFirst(frame_text, "level=",    '"', '"', false);
                QString addr_text     = Seer::parseFirst(frame_text, "addr=",     '"', '"', false);
                QString func_text     = Seer::parseFirst(frame_text, "func=",     '"', '"', false);
                QString file_text     = Seer::parseFirst(frame_text, "file=",     '"', '"', false);
                QString fullname_text = Seer::parseFirst(frame_text, "fullname=", '"', '"', false);
                QString line_text     = Seer::parseFirst(frame_text, "line=",     '"', '"', false);
                QString arch_text     = Seer::parseFirst(frame_text, "arch=",     '"', '"', false);

                SeerEditorManagerEntries::iterator i = findEntry(fullname_text);
                SeerEditorManagerEntries::iterator e = endEntry();

                if (i != e) {
                    i->widget->sourceArea()->addCurrentLine(line_text.toInt());
                }
            }
        }

        // Get the AssemblyWidget.
        SeerEditorWidgetAssembly* assemblyWidget = assemblyWidgetTab();

        if (assemblyWidget) {
            assemblyWidget->assemblyArea()->handleText(text);
            assemblyWidget->handleText(text);
        }

    }else if (text.startsWith("^done,asm_insns=")) {

        // Get the AssemblyWidget.
        SeerEditorWidgetAssembly* assemblyWidget = assemblyWidgetTab();

        if (assemblyWidget) {
            assemblyWidget->assemblyArea()->handleText(text);
            assemblyWidget->handleText(text);
        }

    }else if (text.startsWith("^error,msg=\"No registers.\"")) {

        //qDebug() << text;

        // Clear current lines in all opened editor widgets.
        SeerEditorManagerEntries::iterator b = beginEntry();
        SeerEditorManagerEntries::iterator e = endEntry();

        while (b != e) {
            b->widget->sourceArea()->setCurrentLine(0);
            b++;
        }

    }else if (text.contains(QRegularExpression("^([0-9]+)\\^done,value="))) {

        // 10^done,value="1"
        // 11^done,value="0x7fffffffd538"

        QWidget* w = tabWidget->currentWidget();

        if (w) {
            static_cast<SeerEditorWidgetSource*>(w)->sourceArea()->handleText(text);
        }

        SeerEditorWidgetAssembly* assemblyWidget = assemblyWidgetTab();

        if (assemblyWidget) {
            assemblyWidget->assemblyArea()->handleText(text);
            assemblyWidget->handleText(text);
        }

    }else if (text.contains(QRegularExpression("^([0-9]+)\\^error,msg="))) {

        // 12^error,msg="No symbol \"return\" in current context."
        // 13^error,msg="No symbol \"cout\" in current context."

        QWidget* w = tabWidget->currentWidget();

        if (w) {
            static_cast<SeerEditorWidgetSource*>(w)->sourceArea()->handleText(text);
        }

    }else{
        // Ignore others.
        return;
    }
}

void SeerEditorManagerWidget::handleTabCloseRequested (int index) {

    //qDebug() << index << tabWidget->count() << tabWidget->tabText(0);

    // If it is the place holder, don't delete it.
    if (tabWidget->tabText(index) == "") {
        return;
    }

    // Delete the tab.
    deleteEditorWidgetTab(index);

    // If there are no tabs left, create a place holder.
    if (tabWidget->count() == 0) {
        createEditorWidgetTab("", "");
    }
}

void SeerEditorManagerWidget::handleTabCurrentChanged (int index) {

    if (_notifyAssemblyTabShown == false) {
        return;
    }

    if (tabWidget->tabText(index) == "Assembly") {
        emit assemblyTabShown(true);
    }else{
        emit assemblyTabShown(false);
    }
}

void SeerEditorManagerWidget::handleOpenFile (const QString& file, const QString& fullname, int lineno) {

    // Must have a valid filename.
    if (file == "" || fullname == "") {
        return;
    }

    // Get the EditorWidget for the file. Create one if needed.
    SeerEditorWidgetSource* editorWidget = editorWidgetTab(fullname);

    if (editorWidget == 0) {
        editorWidget = createEditorWidgetTab(fullname, file);
    }

    // Can still be null, if the file is ignored.
    if (editorWidget == 0) {
        return;
    }

    // Push this tab to the top only if the current one in not the "Assembly" tab.
    QString tabtext = "";

    if (tabWidget->currentIndex() >= 0) {
        tabtext = tabWidget->tabText(tabWidget->currentIndex());
    }

    if (keepAssemblyTabOnTop() && tabtext == "Assembly") {
        // Do nothing. The "Assembly" tab is already on top.
    }else{
        tabWidget->setCurrentWidget(editorWidget);
    }

    // If lineno is > 0, set the line number of the editor widget
    if (lineno > 0) {
        editorWidget->sourceArea()->scrollToLine(lineno);
    }

    // Ask for the breakpoint list to be resent, in case this file has breakpoints.
    emit refreshBreakpointsList();

    // Ask for the stackframe list to be resent, in case this file has currently executing lines.
    emit refreshStackFrames();
}

void SeerEditorManagerWidget::handleOpenAddress (const QString& address) {

    // Must have a valid address.
    if (address == "") {
        return;
    }

    // Get the AssemblyWidget so the address can be loaded. Return if there is no widget.
    SeerEditorWidgetAssembly* assemblyWidget = assemblyWidgetTab();

    if (assemblyWidget == 0) {
        return;
    }

    //qDebug() << address;

    assemblyWidget->assemblyArea()->setAddress(address);
}

SeerEditorWidgetSource* SeerEditorManagerWidget::currentEditorWidgetTab () {

    QWidget* w = tabWidget->currentWidget();

    if (w == 0) {
        return 0;
    }

    return dynamic_cast<SeerEditorWidgetSource*>(w);
}

SeerEditorWidgetSource* SeerEditorManagerWidget::editorWidgetTab (const QString& fullname) {

    // Do we have an entry for 'fullname'?
    SeerEditorManagerEntries::iterator i = findEntry(fullname);

    if (i == endEntry()) {
        return 0;
    }

    return i->widget;
}

SeerEditorWidgetSource* SeerEditorManagerWidget::createEditorWidgetTab (const QString& fullname, const QString& file, const QString& text) {

    //qDebug() << "1:" << fullname << file << tabWidget->count() << tabWidget->tabText(0);

    //dumpEntries();

    // Are we asked to ignore this file?
    if (Seer::matchesWildcard(editorIgnoreDirectories(), fullname) == true) {
        emit showMessage(QString("Ignored opening of: '%1'").arg(fullname), 3000);
        return 0;
    }

    // Remove the place holder tab, if present.
    if (tabWidget->count() == 1 && tabWidget->tabText(0) == "") {
        deleteEditorWidgetTab(0);
    }

    // Create the Editor widget and add it to the tab.
    SeerEditorWidgetSource* editorWidget = new SeerEditorWidgetSource(this);
    editorWidget->sourceArea()->setEditorFont(editorFont());
    editorWidget->sourceArea()->setEditorTabSize(editorTabSize());
    editorWidget->sourceArea()->setExternalEditorCommand(editorExternalEditorCommand());
    editorWidget->sourceArea()->setHighlighterSettings(editorHighlighterSettings());
    editorWidget->sourceArea()->setHighlighterEnabled(editorHighlighterEnabled());
    editorWidget->sourceArea()->setAlternateDirectories(editorAlternateDirectories());
    editorWidget->setKeySettings(editorKeySettings());

    // Set the tooltip for the tab.
    int tabno = tabWidget->addTab(editorWidget, QFileInfo(file).fileName());

    tabWidget->setTabToolTip(tabno, QFileInfo(file).fileName() + " : " + fullname);

    // Connect signals.
    QObject::connect(editorWidget->sourceArea(), &SeerEditorWidgetSourceArea::insertBreakpoint,              this, &SeerEditorManagerWidget::handleInsertBreakpoint);
    QObject::connect(editorWidget->sourceArea(), &SeerEditorWidgetSourceArea::insertPrintpoint,              this, &SeerEditorManagerWidget::handleInsertPrintpoint);
    QObject::connect(editorWidget->sourceArea(), &SeerEditorWidgetSourceArea::deleteBreakpoints,             this, &SeerEditorManagerWidget::handleDeleteBreakpoints);
    QObject::connect(editorWidget->sourceArea(), &SeerEditorWidgetSourceArea::enableBreakpoints,             this, &SeerEditorManagerWidget::handleEnableBreakpoints);
    QObject::connect(editorWidget->sourceArea(), &SeerEditorWidgetSourceArea::disableBreakpoints,            this, &SeerEditorManagerWidget::handleDisableBreakpoints);
    QObject::connect(editorWidget->sourceArea(), &SeerEditorWidgetSourceArea::refreshBreakpointsStackFrames, this, &SeerEditorManagerWidget::handleRefreshBreakpointsStackFrames);
    QObject::connect(editorWidget->sourceArea(), &SeerEditorWidgetSourceArea::runToLine,                     this, &SeerEditorManagerWidget::handleRunToLine);
    QObject::connect(editorWidget->sourceArea(), &SeerEditorWidgetSourceArea::addVariableLoggerExpression,   this, &SeerEditorManagerWidget::handleAddVariableLoggerExpression);
    QObject::connect(editorWidget->sourceArea(), &SeerEditorWidgetSourceArea::addVariableTrackerExpression,  this, &SeerEditorManagerWidget::handleAddVariableTrackerExpression);
    QObject::connect(editorWidget->sourceArea(), &SeerEditorWidgetSourceArea::refreshVariableTrackerValues,  this, &SeerEditorManagerWidget::handleRefreshVariableTrackerValues);
    QObject::connect(editorWidget->sourceArea(), &SeerEditorWidgetSourceArea::evaluateVariableExpression,    this, &SeerEditorManagerWidget::handleEvaluateVariableExpression);
    QObject::connect(editorWidget->sourceArea(), &SeerEditorWidgetSourceArea::addMemoryVisualize,            this, &SeerEditorManagerWidget::handleAddMemoryVisualizer);
    QObject::connect(editorWidget->sourceArea(), &SeerEditorWidgetSourceArea::addArrayVisualize,             this, &SeerEditorManagerWidget::handleAddArrayVisualizer);
    QObject::connect(editorWidget->sourceArea(), &SeerEditorWidgetSourceArea::addStructVisualize,            this, &SeerEditorManagerWidget::handleAddStructVisualizer);
    QObject::connect(editorWidget,               &SeerEditorWidgetSource::addAlternateDirectory,             this, &SeerEditorManagerWidget::handleAddAlternateDirectory);

    // Send the Editor widget the command to load the file. ??? Do better than this.
    editorWidget->sourceArea()->handleText(text);

    // Add an entry to our table.
    SeerEditorManagerEntries::iterator i = addEntry(fullname, QFileInfo(file).fileName());
    i->widget = editorWidget;

    // Return the editor widget.
    return i->widget;
}

SeerEditorWidgetSource* SeerEditorManagerWidget::createEditorWidgetTab (const QString& fullname, const QString& file) {

    //qDebug() << "2:" << fullname << file << tabWidget->count() << tabWidget->tabText(0);

    //dumpEntries();

    // Are we asked to ignore this file?
    if (Seer::matchesWildcard(editorIgnoreDirectories(), fullname) == true) {
        emit showMessage(QString("Ignored opening of: '%1'").arg(fullname), 3000);
        return 0;
    }

    // Remove the place holder tab, if present.
    if (tabWidget->count() == 1 && tabWidget->tabText(0) == "") {
        deleteEditorWidgetTab(0);
    }

    // Create the Editor widget and add it to the tab.
    SeerEditorWidgetSource* editorWidget = new SeerEditorWidgetSource(this);
    editorWidget->sourceArea()->setEditorFont(editorFont());
    editorWidget->sourceArea()->setEditorTabSize(editorTabSize());
    editorWidget->sourceArea()->setExternalEditorCommand(editorExternalEditorCommand());
    editorWidget->sourceArea()->setHighlighterSettings(editorHighlighterSettings());
    editorWidget->sourceArea()->setHighlighterEnabled(editorHighlighterEnabled());
    editorWidget->sourceArea()->setAlternateDirectories(editorAlternateDirectories());
    editorWidget->setKeySettings(editorKeySettings());

    // Set the tooltip for the tab.
    int tabno = tabWidget->addTab(editorWidget, QFileInfo(file).fileName());

    tabWidget->setTabToolTip(tabno, QFileInfo(file).fileName() + " : " + fullname);

    // Connect signals.
    QObject::connect(editorWidget->sourceArea(), &SeerEditorWidgetSourceArea::insertBreakpoint,              this, &SeerEditorManagerWidget::handleInsertBreakpoint);
    QObject::connect(editorWidget->sourceArea(), &SeerEditorWidgetSourceArea::insertPrintpoint,              this, &SeerEditorManagerWidget::handleInsertPrintpoint);
    QObject::connect(editorWidget->sourceArea(), &SeerEditorWidgetSourceArea::deleteBreakpoints,             this, &SeerEditorManagerWidget::handleDeleteBreakpoints);
    QObject::connect(editorWidget->sourceArea(), &SeerEditorWidgetSourceArea::enableBreakpoints,             this, &SeerEditorManagerWidget::handleEnableBreakpoints);
    QObject::connect(editorWidget->sourceArea(), &SeerEditorWidgetSourceArea::disableBreakpoints,            this, &SeerEditorManagerWidget::handleDisableBreakpoints);
    QObject::connect(editorWidget->sourceArea(), &SeerEditorWidgetSourceArea::refreshBreakpointsStackFrames, this, &SeerEditorManagerWidget::handleRefreshBreakpointsStackFrames);
    QObject::connect(editorWidget->sourceArea(), &SeerEditorWidgetSourceArea::runToLine,                     this, &SeerEditorManagerWidget::handleRunToLine);
    QObject::connect(editorWidget->sourceArea(), &SeerEditorWidgetSourceArea::addVariableLoggerExpression,   this, &SeerEditorManagerWidget::handleAddVariableLoggerExpression);
    QObject::connect(editorWidget->sourceArea(), &SeerEditorWidgetSourceArea::addVariableTrackerExpression,  this, &SeerEditorManagerWidget::handleAddVariableTrackerExpression);
    QObject::connect(editorWidget->sourceArea(), &SeerEditorWidgetSourceArea::refreshVariableTrackerValues,  this, &SeerEditorManagerWidget::handleRefreshVariableTrackerValues);
    QObject::connect(editorWidget->sourceArea(), &SeerEditorWidgetSourceArea::evaluateVariableExpression,    this, &SeerEditorManagerWidget::handleEvaluateVariableExpression);
    QObject::connect(editorWidget->sourceArea(), &SeerEditorWidgetSourceArea::addMemoryVisualize,            this, &SeerEditorManagerWidget::handleAddMemoryVisualizer);
    QObject::connect(editorWidget->sourceArea(), &SeerEditorWidgetSourceArea::addArrayVisualize,             this, &SeerEditorManagerWidget::handleAddArrayVisualizer);
    QObject::connect(editorWidget->sourceArea(), &SeerEditorWidgetSourceArea::addStructVisualize,            this, &SeerEditorManagerWidget::handleAddStructVisualizer);
    QObject::connect(editorWidget,               &SeerEditorWidgetSource::addAlternateDirectory,             this, &SeerEditorManagerWidget::handleAddAlternateDirectory);

    // Load the file.
    editorWidget->sourceArea()->open(fullname, QFileInfo(file).fileName());

    // Add an entry to our table.
    SeerEditorManagerEntries::iterator i = addEntry(fullname, QFileInfo(file).fileName());
    i->widget = editorWidget;

    // Return the editor widget.
    return i->widget;
}

void SeerEditorManagerWidget::deleteEditorWidgetTab (int index) {

    //qDebug() << index << tabWidget->count() << tabWidget->tabText(index);

    // Get the editor widget. Try as a SeerEditorWidgetSource.
    SeerEditorWidgetSource* editorWidget = dynamic_cast<SeerEditorWidgetSource*>(tabWidget->widget(index));
    if (editorWidget != 0) {

        // Look for the matching entry for the EditorWidget.
        // If found, delete it and clean up the map.
        SeerEditorManagerEntries::iterator b = beginEntry();
        SeerEditorManagerEntries::iterator e = endEntry();

        while (b != e) {
            if (editorWidget == b->widget) {

                deleteEntry(b);                 // Delete the entry from the map.
                tabWidget->removeTab(index);    // Remove the tab.
                delete editorWidget;            // Delete the actual EditorWidget

                break;
            }

            b++;
        }

        return;
    }

    // Get the editor widget. Try as a SeerEditorWidgetAssembly.
    SeerEditorWidgetAssembly* assemblyWidget = dynamic_cast<SeerEditorWidgetAssembly*>(tabWidget->widget(index));
    if (assemblyWidget != 0) {

        deleteAssemblyWidgetTab();

        return;
    }
}

SeerEditorWidgetAssembly* SeerEditorManagerWidget::createAssemblyWidgetTab () {

    // Does it already exist?
    if (assemblyWidgetTab() != 0) {
        return assemblyWidgetTab();
    }

    //qDebug() << tabWidget->count() << tabWidget->tabText(0);

    // Remove the place holder tab, if present.
    if (tabWidget->count() == 1 && tabWidget->tabText(0) == "") {
        deleteEditorWidgetTab(0);
    }

    // Create the Editor widget and add it to the tab.
    // Raise it to the top.
    SeerEditorWidgetAssembly* assemblyWidget = new SeerEditorWidgetAssembly(this);
    assemblyWidget->assemblyArea()->setEditorFont(editorFont());
    assemblyWidget->assemblyArea()->setEditorTabSize(editorTabSize());
    assemblyWidget->assemblyArea()->setHighlighterSettings(editorHighlighterSettings());
    assemblyWidget->assemblyArea()->setHighlighterEnabled(editorHighlighterEnabled());

    assemblyWidget->setShowAddressColumn(assemblyShowAddressColumn());
    assemblyWidget->setShowOffsetColumn(assemblyShowOffsetColumn());
    assemblyWidget->setShowOpcodeColumn(assemblyShowOpcodeColumn());
    assemblyWidget->setShowSourceLines(assemblyShowSourceLines());

    _assemblyIndex= tabWidget->addTab(assemblyWidget, "Assembly");

    tabWidget->setCurrentWidget(assemblyWidget);

    // Set the tooltip for the tab.
    tabWidget->setTabToolTip(_assemblyIndex, "Assembly");

    // Connect signals.
    QObject::connect(assemblyWidget->assemblyArea(), &SeerEditorWidgetAssemblyArea::insertBreakpoint,               this, &SeerEditorManagerWidget::handleInsertBreakpoint);
    QObject::connect(assemblyWidget->assemblyArea(), &SeerEditorWidgetAssemblyArea::deleteBreakpoints,              this, &SeerEditorManagerWidget::handleDeleteBreakpoints);
    QObject::connect(assemblyWidget->assemblyArea(), &SeerEditorWidgetAssemblyArea::enableBreakpoints,              this, &SeerEditorManagerWidget::handleEnableBreakpoints);
    QObject::connect(assemblyWidget->assemblyArea(), &SeerEditorWidgetAssemblyArea::disableBreakpoints,             this, &SeerEditorManagerWidget::handleDisableBreakpoints);
    QObject::connect(assemblyWidget->assemblyArea(), &SeerEditorWidgetAssemblyArea::refreshBreakpointsStackFrames,  this, &SeerEditorManagerWidget::handleRefreshBreakpointsStackFrames);
    QObject::connect(assemblyWidget->assemblyArea(), &SeerEditorWidgetAssemblyArea::runToAddress,                   this, &SeerEditorManagerWidget::handleRunToAddress);
    QObject::connect(assemblyWidget->assemblyArea(), &SeerEditorWidgetAssemblyArea::requestAssembly,                this, &SeerEditorManagerWidget::handleRequestAssembly);
    QObject::connect(assemblyWidget->assemblyArea(), &SeerEditorWidgetAssemblyArea::requestSourceAndAssembly,       this, &SeerEditorManagerWidget::handleRequestSourceAndAssembly);
    QObject::connect(assemblyWidget->assemblyArea(), &SeerEditorWidgetAssemblyArea::addMemoryVisualize,             this, &SeerEditorManagerWidget::handleAddMemoryVisualizer);
    QObject::connect(assemblyWidget->assemblyArea(), &SeerEditorWidgetAssemblyArea::addArrayVisualize,              this, &SeerEditorManagerWidget::handleAddArrayVisualizer);
    QObject::connect(assemblyWidget->assemblyArea(), &SeerEditorWidgetAssemblyArea::addStructVisualize,             this, &SeerEditorManagerWidget::handleAddStructVisualizer);
    QObject::connect(assemblyWidget,                 &SeerEditorWidgetAssembly::evaluateVariableExpression,         this, &SeerEditorManagerWidget::handleEvaluateVariableExpression);

    // Load the file.
    assemblyWidget->assemblyArea()->setPlainText("");

    // Add an entry to our table.
    _assemblyWidget = assemblyWidget;

    // Send a signal to enable the Nexti/Stepi buttons.
    emit assemblyTabShown(true);

    // Return the editor widget.
    return assemblyWidget;
}

void SeerEditorManagerWidget::deleteAssemblyWidgetTab () {

    if (_assemblyWidget == 0) {
        return;
    }

    //qDebug() << "Delete AssemblyWidget";

    tabWidget->removeTab(_assemblyIndex);   // Remove the tab.
    delete _assemblyWidget;                 // Delete the actual EditorWidget

    _assemblyWidget = 0;

    // Send a signal to disable the Nexti/Stepi buttons.
    if (_notifyAssemblyTabShown) {
        emit assemblyTabShown(false);
    }
}

void SeerEditorManagerWidget::handleFileOpenToolButtonClicked () {

    QString filename = QFileDialog::getOpenFileName(this, tr("Open Source File"), "./", tr("Source files (*.*)"), nullptr, QFileDialog::DontUseNativeDialog);

    if (filename == "") {
        return;
    }

    QFileInfo info(filename);
    info.makeAbsolute();

    handleOpenFile(info.fileName(), info.filePath(), 0);
}

void SeerEditorManagerWidget::handleFileCloseToolButtonClicked () {

    SeerCloseSourceDialog dlg(this);
    dlg.setFiles(openedFiles());

    int ret = dlg.exec();

    if (ret == 0) {
        return;
    }

    SeerEditorManagerFiles files = dlg.selectedFiles();

    for (int i=0; i<files.size(); i++) {

        SeerEditorWidgetSource* w = editorWidgetTab(files[i].fullname);

        if (w == 0) {
            qWarning() << "Can't find opened file for:" << files[i].fullname;
            continue;
        }

        int idx = tabWidget->indexOf(w);

        if (idx < 0) {
            qWarning() << "Can't find tab index for:" << files[i].fullname;
            continue;
        }

        deleteEditorWidgetTab(idx);
    }

    // If there are no tabs left, create a place holder.
    if (tabWidget->count() == 0) {
        createEditorWidgetTab("", "");
    }
}

void SeerEditorManagerWidget::handleTextSearchToolButtonClicked () {

    SeerEditorWidgetSource* w = currentEditorWidgetTab();

    if (w == 0) {
        return;
    }

    if (w->isSearchBarShown() == true) {
        w->showSearchBar(false);
    }else{
        w->showSearchBar(true);
    }
}

void SeerEditorManagerWidget::handleHelpToolButtonClicked () {

    SeerHelpPageDialog* help = new SeerHelpPageDialog;
    help->loadFile(":/seer/resources/help/CodeManager.md");
    help->show();
    help->raise();
}

void SeerEditorManagerWidget::handleAddAlternateDirectory (QString path) {

    // Don't re-add it if it already exists in the list.
    if (_editorAlternateDirectories.contains(path) == true) {

        QMessageBox::warning(this, "Note.", "The directory '" + path +"' is already in the alternate directories list.");

        return;
    }

    // Add the new path to our list.
    _editorAlternateDirectories << path;

    // Update any open editors. Future editors will get the updated list normally.
    SeerEditorManagerEntries::iterator b = beginEntry();
    SeerEditorManagerEntries::iterator e = endEntry();

    while (b != e) {
        b->widget->sourceArea()->setAlternateDirectories(_editorAlternateDirectories);
        b++;
    }

    QMessageBox::information(this, "Note.", "Added '" + path +"' to the alternate directories list.");
}

void SeerEditorManagerWidget::handleInsertBreakpoint (QString breakpoint) {

    //qDebug() << breakpoint;

    // rethrow
    emit insertBreakpoint (breakpoint);
}

void SeerEditorManagerWidget::handleInsertPrintpoint (QString printpoint) {

    //qDebug() << printpoint;

    // rethrow
    emit insertPrintpoint (printpoint);
}

void SeerEditorManagerWidget::handleDeleteBreakpoints (QString breakpoints) {

    //qDebug() << breakpoints;

    // rethrow
    emit deleteBreakpoints (breakpoints);
}

void SeerEditorManagerWidget::handleEnableBreakpoints (QString breakpoints) {

    //qDebug() << breakpoints;

    // rethrow
    emit enableBreakpoints (breakpoints);
}

void SeerEditorManagerWidget::handleDisableBreakpoints (QString breakpoints) {

    //qDebug() << breakpoints;

    // rethrow
    emit disableBreakpoints (breakpoints);
}

void SeerEditorManagerWidget::handleRefreshBreakpointsStackFrames () {

    // Ask for the breakpoint list to be resent, in case files have breakpoints.
    emit refreshBreakpointsList();

    // Ask for the stackframe list to be resent, in case files have currently executing lines.
    emit refreshStackFrames();
}

void SeerEditorManagerWidget::handleRunToLine (QString fullname, int lineno) {

    //qDebug() << fullname << lineno;

    // rethrow
    emit runToLine (fullname, lineno);
}

void SeerEditorManagerWidget::handleRunToAddress (QString address) {

    //qDebug() << address;

    // rethrow
    emit runToAddress (address);
}

void SeerEditorManagerWidget::handleAddVariableLoggerExpression (QString expression) {

    //qDebug() << expression;

    // rethrow
    emit addVariableLoggerExpression (expression);
}

void SeerEditorManagerWidget::handleAddVariableTrackerExpression (QString expression) {

    //qDebug() << expression;

    // rethrow
    emit addVariableTrackerExpression (expression);
}

void SeerEditorManagerWidget::handleRefreshVariableTrackerValues () {

    //qDebug();

    // rethrow
    emit refreshVariableTrackerValues ();
}

void SeerEditorManagerWidget::handleEvaluateVariableExpression (int expressionid, QString expression) {

    //qDebug();

    // rethrow
    emit evaluateVariableExpression (expressionid, expression);
}

void SeerEditorManagerWidget::handleAddMemoryVisualizer (QString expression) {

    //qDebug() << expression;

    // rethrow
    emit addMemoryVisualize (expression);
}

void SeerEditorManagerWidget::handleAddArrayVisualizer (QString expression) {

    //qDebug() << expression;

    // rethrow
    emit addArrayVisualize (expression);
}

void SeerEditorManagerWidget::handleAddStructVisualizer (QString expression) {

    //qDebug() << expression;

    // rethrow
    emit addStructVisualize (expression);
}

void SeerEditorManagerWidget::handleRequestAssembly (QString address) {

    //qDebug() << address;

    // rethrow
    emit requestAssembly (address);

    // Ask for the stackframe list to be resent, this will highlight current line when the
    // assembly tab is shown for the first time.
    emit refreshStackFrames();
}

void SeerEditorManagerWidget::handleRequestSourceAndAssembly (QString address) {

    //qDebug() << address;

    // rethrow
    emit requestSourceAndAssembly (address);

    // Ask for the stackframe list to be resent, this will highlight current line when the
    // assembly tab is shown for the first time.
    emit refreshStackFrames();
}

void SeerEditorManagerWidget::handleAssemblyConfigChanged () {

    // Tell the assembly tab to refresh.
    if (assemblyWidgetTab() != 0) {
        assemblyWidgetTab()->reloadAssembly();
    }
}


