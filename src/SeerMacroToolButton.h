// SPDX-FileCopyrightText: 2026 Ernie Pasveer <epasveer@att.net>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#pragma once

#include <QtWidgets/QToolButton>
#include <QtCore/QString>
#include <QtCore/QStringList>

class SeerMacroToolButton : public QToolButton {

    Q_OBJECT

    public:
        explicit SeerMacroToolButton(QWidget* parent = nullptr);

        void                    setMacroName            (const QString& name);
        const QString&          macroName               () const;

        void                    setCommands             (const QStringList& commands);
        const QStringList&      commands                () const;

    protected:
        void                    mousePressEvent         (QMouseEvent* event) override;
        void                    mouseReleaseEvent       (QMouseEvent* event) override;

    signals:

    private slots:
        void                    handleHoldTriggered     ();
        void                    handleEditMacro         ();

    private:
        QTimer*                 _holdTimer;
        QMenu*                  _menu;
        QPoint                  _pressPos;
        QString                 _macroName;
        QStringList             _commands;
};

