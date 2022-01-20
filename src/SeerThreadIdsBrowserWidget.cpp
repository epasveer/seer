#include "SeerThreadIdsBrowserWidget.h"
#include "SeerUtl.h"
#include <QtWidgets/QTreeWidget>
#include <QtWidgets/QTreeWidgetItemIterator>
#include <QtWidgets/QApplication>
#include <QtGui/QFont>
#include <QtCore/QDebug>

SeerThreadIdsBrowserWidget::SeerThreadIdsBrowserWidget (QWidget* parent) : QWidget(parent) {

    // Construct the UI.
    setupUi(this);

    // Setup the widgets
    idsTreeWidget->setSortingEnabled(false);
    idsTreeWidget->resizeColumnToContents(0); // id

    idsTreeWidget->clear();

    // Connect things.
    QObject::connect(idsTreeWidget,   &QTreeWidget::itemDoubleClicked,    this,  &SeerThreadIdsBrowserWidget::handleItemDoubleClicked);
}

SeerThreadIdsBrowserWidget::~SeerThreadIdsBrowserWidget () {
}

void SeerThreadIdsBrowserWidget::handleText (const QString& text) {

    // Don't do any work if the widget is hidden.
    if (isHidden()) {
        return;
    }

    QApplication::setOverrideCursor(Qt::BusyCursor);

    if (text.startsWith("^done,thread-ids={")) {

        QString newtext = Seer::filterEscapes(text); // Filter escaped characters.

        // ^done,thread-ids={
        //        thread-id=\"1\",
        //        thread-id=\"2\"
        //    },
        //    current-thread-id=\"1\",
        //    number-of-threads=\"2\"

        idsTreeWidget->clear();

        QString threadids_text       = Seer::parseFirst(newtext,   "thread-ids=",        '{', '}', false);
        QStringList threadids_list   = Seer::parse(threadids_text, "thread-id=",         '"', '"', false);
        QString currentthreadid_text = Seer::parseFirst(newtext,   "current-thread-id=", '"', '"', false);

        // Add the thread-ids.
        for ( const auto& threadid_text : threadids_list  ) {

            //qDebug() << threadid_text;

            // Construct the item
            QTreeWidgetItem* item = new QTreeWidgetItem;
            item->setText(0, threadid_text);

            // Set the text to bold if the ID is the same as the CURRENT ID.
            QFont fnormal = item->font(0); fnormal.setBold(false);
            QFont fbold   = item->font(0); fbold.setBold(true);

            if (threadid_text == currentthreadid_text) {
                item->setFont(0, fbold);
            }else{
                item->setFont(0, fnormal);
            }

            // Add the frame to the tree.
            idsTreeWidget->addTopLevelItem(item);
        }

        // Clear the selection and select the one for the current thread-id.
        idsTreeWidget->clearSelection();

        QList<QTreeWidgetItem*> matches = idsTreeWidget->findItems(currentthreadid_text, Qt::MatchExactly, 0);
        if (matches.size() > 0) {
            idsTreeWidget->setCurrentItem(matches.first());
        }

    }else if (text.startsWith("^error,msg=\"No registers.\"")) {
        idsTreeWidget->clear();

    }else{
        // Ignore others.
    }

    idsTreeWidget->resizeColumnToContents(0);

    QApplication::restoreOverrideCursor();
}

void SeerThreadIdsBrowserWidget::handleStoppingPointReached () {

    // Don't do any work if the widget is hidden.
    if (isHidden()) {
        return;
    }

    refresh();
}

void SeerThreadIdsBrowserWidget::handleItemDoubleClicked (QTreeWidgetItem* item, int column) {

    Q_UNUSED(column);

    emit selectedThread(item->text(0).toInt());
}

void SeerThreadIdsBrowserWidget::refresh () {
    emit refreshThreadIds();
}

void SeerThreadIdsBrowserWidget::showEvent (QShowEvent* event) {

    QWidget::showEvent(event);

    refresh();
}

