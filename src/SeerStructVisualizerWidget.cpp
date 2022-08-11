#include "SeerStructVisualizerWidget.h"
#include "SeerUtl.h"
#include <QtWidgets/QTreeWidget>
#include <QtWidgets/QTreeWidgetItemIterator>
#include <QtGui/QIcon>
#include <QtCore/QRegExp>
#include <QtCore/QTime>
#include <QtCore/QSettings>
#include <QtCore/QDebug>

SeerStructVisualizerWidget::SeerStructVisualizerWidget (QWidget* parent) : QWidget(parent) {

    // Init variables.
    _variableId = Seer::createID(); // Create id for queries.

    // Set up UI.
    setupUi(this);

    // Setup the widgets
    setWindowIcon(QIcon(":/seer/resources/seer_64x64.png"));
    setWindowTitle("Seer Struct Visualizer");

    variableTreeWidget->setMouseTracking(true);
    variableTreeWidget->setSortingEnabled(false);
    variableTreeWidget->setSelectionMode(QAbstractItemView::ExtendedSelection);
    variableTreeWidget->setRootIsDecorated(true);
    variableTreeWidget->setItemsExpandable(true);
    variableTreeWidget->resizeColumnToContents(0); // name
    variableTreeWidget->resizeColumnToContents(1); // value
    variableTreeWidget->clear();

    // Connect things.
    QObject::connect(refreshToolButton,      &QToolButton::clicked,              this,  &SeerStructVisualizerWidget::handleRefreshButton);
    QObject::connect(variableNameLineEdit,   &QLineEdit::returnPressed,          this,  &SeerStructVisualizerWidget::handleVariableNameLineEdit);
    QObject::connect(variableTreeWidget,     &QTreeWidget::itemEntered,          this,  &SeerStructVisualizerWidget::handleItemEntered);
    QObject::connect(variableTreeWidget,     &QTreeWidget::itemExpanded,         this,  &SeerStructVisualizerWidget::handleItemExpanded);
    QObject::connect(variableTreeWidget,     &QTreeWidget::itemCollapsed,        this,  &SeerStructVisualizerWidget::handleItemExpanded);

    // Restore window settings.
    readSettings();
}

SeerStructVisualizerWidget::~SeerStructVisualizerWidget () {
}

void SeerStructVisualizerWidget::setVariableName (const QString& name) {

    setWindowTitle("Seer Struct Visualizer - '" + name + "'");

    variableNameLineEdit->setText(name);

    // Create the initial variable in the tree.
    variableTreeWidget->clear();

    if (variableNameLineEdit->text() != "") {
        QTreeWidgetItem* item = new QTreeWidgetItem;
        item->setText(0, name);
        item->setText(1, "");

        variableTreeWidget->addTopLevelItem(item);
    }

    // Resize columns.
    variableTreeWidget->resizeColumnToContents(0);
    variableTreeWidget->resizeColumnToContents(1);

    // Send signal to get variable result.
    if (variableNameLineEdit->text() != "") {
        emit evaluateVariableExpression(_variableId, variableNameLineEdit->text());
    }
}

QString SeerStructVisualizerWidget::variableName () const {
    return variableNameLineEdit->text();
}

void SeerStructVisualizerWidget::handleText (const QString& text) {

    QApplication::setOverrideCursor(Qt::BusyCursor);

    if (text.contains(QRegExp("^([0-9]+)\\^done,value="))) {

        QString id_text    = text.section('^', 0,0);
        QString value_text = Seer::parseFirst(text,       "value=", '"', '"', false);

        if (id_text.toInt() == _variableId) {

            QTreeWidgetItem* topItem = variableTreeWidget->topLevelItem(0);
            if (topItem == 0) {
                return;
            }

            // Delete all subitems of the toplevel item.
            foreach (auto item, topItem->takeChildren()) delete item;

            // Populate the tree.
            handleItemCreate(topItem, value_text);

            // For now, always expand everything.
            variableTreeWidget->expandAll();
        }


    }else if (text.contains(QRegExp("^([0-9]+)\\^error,msg="))) {

        QString id_text  = text.section('^', 0,0);
        QString msg_text = Seer::parseFirst(text, "msg=", '"', '"', false);

        if (id_text.toInt() == _variableId) {

            QTreeWidgetItem* topItem = variableTreeWidget->topLevelItem(0);
            if (topItem == 0) {
                return;
            }

            // Delete all subitems of the toplevel item.
            foreach (auto item, topItem->takeChildren()) delete item;

            // Set the error text.
            topItem->setText(1, Seer::filterEscapes(msg_text));
        }


    }else if (text.startsWith("^error,msg=\"No registers.\"")) {
        variableTreeWidget->clear();

    // At a stopping point, refresh.
    }else if (text.startsWith("*stopped,reason=\"")) {

        if (autoRefreshCheckBox->isChecked()) {
            handleRefreshButton();
        }

    }else{
        // Ignore anything else.
    }

    // Resize columns.
    variableTreeWidget->resizeColumnToContents(0);
    variableTreeWidget->resizeColumnToContents(1);

    // Set the cursor back.
    QApplication::restoreOverrideCursor();
}

void SeerStructVisualizerWidget::handleItemCreate (QTreeWidgetItem* parentItem, const QString& value_text) {

    if (Seer::hasBookends(value_text, '{', '}')) {

        // Remove bookends
        QString text = Seer::filterBookends(value_text, '{', '}');

        // Set the flatvalue text.
        parentItem->setText(1, Seer::filterEscapes(text));

        // Convert to a list of name/value pairs.
        QStringList nv_pairs = Seer::parseCommaList(text, '{', '}');

        // Go through each pair and add the name and its value to the tree.
        QTreeWidgetItem* prevItem = 0;

        for (const auto& nv : nv_pairs) {

            QStringPair pair = Seer::parseNameValue(nv, '=');

            // Handle "name = "xxxx", '\\000' <repeats 14 times>, "yyyy" ..., age = 0, salary = 0"
            // There is a 'first' value but no 'second'. So just concatenate unto the previous item.
            if (pair.second == "") {

                if (prevItem) {
                    prevItem->setText(0, prevItem->text(0) + pair.first);
                    prevItem->setText(1, "");
                }

            // Normal case of "name = value".
            }else{

                QTreeWidgetItem* item = new QTreeWidgetItem;
                item->setText(0, pair.first);
                item->setText(1, "");

                parentItem->addChild(item);

                // Handle recursion if value has bookends.
                if (Seer::hasBookends(pair.second, '{', '}')) {
                    handleItemCreate(item, pair.second);
                }else{
                    item->setText(1, Seer::filterEscapes(pair.second));
                }

                prevItem = item;
            }
        }

    }else{
        parentItem->setText(1, Seer::filterEscapes(value_text));
    }
}

void SeerStructVisualizerWidget::handleItemEntered (QTreeWidgetItem* item, int column) {

    Q_UNUSED(column);

    item->setToolTip(0, item->text(0) + " : " + item->text(1));

    for (int i=1; i<variableTreeWidget->columnCount(); i++) { // Copy tooltip to other columns.
        item->setToolTip(i, item->toolTip(0));
    }
}

void SeerStructVisualizerWidget::handleItemExpanded (QTreeWidgetItem* item) {

    Q_UNUSED(item);

    // Resize columns.
    variableTreeWidget->resizeColumnToContents(0);
    variableTreeWidget->resizeColumnToContents(1);
}

void SeerStructVisualizerWidget::handleRefreshButton () {

    if (variableNameLineEdit->text() == "") {
        return;
    }

    // Send signal to get variable result.
    emit evaluateVariableExpression(_variableId, variableNameLineEdit->text());
}

void SeerStructVisualizerWidget::handleVariableNameLineEdit () {

    if (variableNameLineEdit->text() == "") {
        return;
    }

    setVariableName (variableNameLineEdit->text());
}

void SeerStructVisualizerWidget::writeSettings () {

    QSettings settings;

    settings.beginGroup("structvisualizerwindow");
    settings.setValue("size", size());
    settings.endGroup();
}

void SeerStructVisualizerWidget::readSettings () {

    QSettings settings;

    settings.beginGroup("structvisualizerwindow");
    resize(settings.value("size", QSize(800, 400)).toSize());
    settings.endGroup();
}

void SeerStructVisualizerWidget::resizeEvent (QResizeEvent* event) {

    writeSettings();

    QWidget::resizeEvent(event);
}

