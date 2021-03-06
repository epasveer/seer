#pragma once

#include "SeerEditorWidgetSource.h"
#include <QtCore/QString>
#include <QtCore/QMap>
#include <QtCore/QVector>

struct SeerEditorManagerEntry {
    QString                   fullname;
    QString                   file;
    SeerEditorWidgetSource*   widget;
};

typedef QMap<QString,SeerEditorManagerEntry> SeerEditorManagerEntries;

struct SeerEditorManagerFile {
    QString             fullname;
    QString             file;
};

typedef QVector<SeerEditorManagerFile> SeerEditorManagerFiles;

