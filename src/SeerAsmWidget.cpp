#include "SeerAsmWidget.h"
#include "SeerUtl.h"
#include <QtCore/QDebug>

SeerAsmWidget::SeerAsmWidget(QWidget* parent) : QWidget(parent) {

    // Construct the UI.
    setupUi(this);

    // Setup the widgets
    QFont font;
    font.setFamily("monospace [Consolas]");
    font.setFixedPitch(true);
    font.setStyleHint(QFont::TypeWriter);

    treeWidget->setFont(font);
    treeWidget->setRootIsDecorated(false);

    // Connect things.
}

SeerAsmWidget::~SeerAsmWidget() {
}

void SeerAsmWidget::setData (const QString& data) {


    // ^done,asm_insns=[
    //                  {address="0x000000000040093c",func-name="main()",offset="362",inst="mov    $0x400aa9,%edi"},
    //                  {address="0x0000000000400941",func-name="main()",offset="367",inst="call   0x400680 <puts@plt>"},
    //                  {address="0x0000000000400946",func-name="main()",offset="372",inst="mov    -0x30(%rbp),%rax"}
    //                 ]

    treeWidget->clear();

    QString asm_insns_text = Seer::parseFirst(data, "asm_insns=", '[', ']', false);

    QStringList asm_list = Seer::parse(asm_insns_text, "", '{', '}', false);

    // Loop through the asm list.
    for ( const auto& asm_text : asm_list  ) {

        QString address_text = Seer::parseFirst(asm_text, "address=", '"', '"', false);
        QString opcodes_text = Seer::parseFirst(asm_text, "opcodes=", '"', '"', false);
        QString inst_text    = Seer::parseFirst(asm_text, "inst=",    '"', '"', false);

        // Add the file to the tree.
        QTreeWidgetItem* item = new QTreeWidgetItem;
        item->setText(0, address_text);
        item->setText(1, opcodes_text);
        item->setText(2, inst_text);

        treeWidget->addTopLevelItem(item);
    }

    treeWidget->resizeColumnToContents(0);
    treeWidget->resizeColumnToContents(1);
    treeWidget->resizeColumnToContents(2);
}

