#pragma once

#include "SeerEditorWidget.h"
#include <QtCore/QString>
#include <QtCore/QMap>

struct SeerEditorManagerEntry {
    QString             fullname;
    QString             file;
    SeerEditorWidget*   widget;
};

typedef QMap<QString,SeerEditorManagerEntry> SeerEditorManagerEntries;

