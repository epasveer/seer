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
    variableTreeWidget->resizeColumnToContents(0); // id
    variableTreeWidget->resizeColumnToContents(1); // timestamp
    variableTreeWidget->resizeColumnToContents(2); // name
    variableTreeWidget->resizeColumnToContents(3); // value
    variableTreeWidget->setColumnHidden(0, true);  // Hide the 'number' column.
    variableTreeWidget->clear();

    // Connect things.
    QObject::connect(refreshToolButton,             &QToolButton::clicked,                                     this,  &SeerStructVisualizerWidget::handleRefreshButton);
    QObject::connect(variableNameLineEdit,          &QLineEdit::returnPressed,                                 this,  &SeerStructVisualizerWidget::handleVariableNameLineEdit);
    QObject::connect(variableTreeWidget,            &QTreeWidget::itemEntered,                                 this,  &SeerStructVisualizerWidget::handleItemEntered);

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
        item->setText(0, QString::number(_variableId));
        item->setText(1, QTime::currentTime().toString(Qt::TextDate));
        item->setText(2, name);
        item->setText(3, "");

        variableTreeWidget->addTopLevelItem(item);
    }

    // Resize columns.
    variableTreeWidget->resizeColumnToContents(0);
    variableTreeWidget->resizeColumnToContents(1);
    variableTreeWidget->resizeColumnToContents(2);
    variableTreeWidget->resizeColumnToContents(3);

    // Send signal to get variable result.
    if (variableNameLineEdit->text() != "") {
        emit evaluateVariableExpression(_variableId, variableNameLineEdit->text());
    }
}

QString SeerStructVisualizerWidget::variableName () const {
    return variableNameLineEdit->text();
}

void SeerStructVisualizerWidget::handleText (const QString& text) {

    qDebug() << text;

    QApplication::setOverrideCursor(Qt::BusyCursor);

    if (text.contains(QRegExp("^([0-9]+)\\^done,value="))) {

        QString id_text    = text.section('^', 0,0);
        QString value_text = Seer::parseFirst(text,       "value=", '"', '"', false);
        QString xxxxx_text = Seer::parseFirst(value_text, "",       '{', '}', false);

        qDebug() << xxxxx_text;

        if (id_text.toInt() == _variableId) {
            QList<QTreeWidgetItem*> matches = variableTreeWidget->findItems(id_text, Qt::MatchExactly, 0);

            if (matches.size() > 0) {
                matches.first()->setText(1, QTime::currentTime().toString(Qt::TextDate));
                matches.first()->setText(3, Seer::filterEscapes(value_text));
            }
        }


    }else if (text.contains(QRegExp("^([0-9]+)\\^error,msg="))) {

        QString id_text  = text.section('^', 0,0);
        QString msg_text = Seer::parseFirst(text, "msg=", '"', '"', false);

        if (id_text.toInt() == _variableId) {
            QList<QTreeWidgetItem*> matches = variableTreeWidget->findItems(id_text, Qt::MatchExactly, 0);

            if (matches.size() > 0) {
                matches.first()->setText(1, QTime::currentTime().toString(Qt::TextDate));
                matches.first()->setText(3, Seer::filterEscapes(msg_text));
            }
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
    variableTreeWidget->resizeColumnToContents(2);
    variableTreeWidget->resizeColumnToContents(3);

    // Set the cursor back.
    QApplication::restoreOverrideCursor();
}

void SeerStructVisualizerWidget::handleItemEntered (QTreeWidgetItem* item, int column) {

    Q_UNUSED(column);

    //qDebug() << item->text(0) << column;

    item->setToolTip(0, item->text(1) + " : " + item->text(2) + " : " + item->text(3));

    for (int i=1; i<variableTreeWidget->columnCount(); i++) { // Copy tooltip to other columns.
        item->setToolTip(i, item->toolTip(0));
    }
}

void SeerStructVisualizerWidget::handleRefreshButton () {

    if (variableNameLineEdit->text() == "") {
        return;
    }

    qDebug();

    // Send signal to get variable result.
    emit evaluateVariableExpression(_variableId, variableNameLineEdit->text());
}

void SeerStructVisualizerWidget::handleVariableNameLineEdit () {

    setVariableName (variableNameLineEdit->text());
}

void SeerStructVisualizerWidget::writeSettings() {

    QSettings settings;

    settings.beginGroup("structvisualizerwindow");
    settings.setValue("size", size());
    settings.endGroup();
}

void SeerStructVisualizerWidget::readSettings() {

    QSettings settings;

    settings.beginGroup("structvisualizerwindow");
    resize(settings.value("size", QSize(800, 400)).toSize());
    settings.endGroup();
}

void SeerStructVisualizerWidget::resizeEvent (QResizeEvent* event) {

    writeSettings();

    QWidget::resizeEvent(event);
}

