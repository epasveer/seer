#pragma once

#include <QtWidgets/QFileSystemModel>
#include <QtCore/QSortFilterProxyModel>
#include <QtCore/QDebug>

class SeerExecutableFilterProxyModel : public QSortFilterProxyModel {

    bool filterAcceptsRow (int sourceRow, const QModelIndex& sourceParent) const {

        QFileSystemModel* fileModel = qobject_cast<QFileSystemModel*>(sourceModel());

        QFileInfo file(fileModel->filePath(sourceModel()->index(sourceRow, 0, sourceParent)));

        /*
        if (fileModel != NULL) {
            return false;
        }
        */

        if (file.isExecutable() == true || file.isHidden()) {
            return true;
        }else{
            return false;
        }
    }
};

