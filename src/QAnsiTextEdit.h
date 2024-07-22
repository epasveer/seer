#pragma once

#include <QtWidgets/QPlainTextEdit>
#include <QtWidgets/QWidget>
#include <QtGui/QColor>
#include <QtGui/QTextCharFormat>
#include <QtCore/QString>
#include <QtCore/QList>

class QAnsiTextEditFormattedText {
    public:
        QAnsiTextEditFormattedText() = default;
        QAnsiTextEditFormattedText(const QString& txt, const QTextCharFormat& fmt = QTextCharFormat());

        QString         text;
        QTextCharFormat format;
};

class QAnsiTextEditEscapeCodeHandler {
    public:
        QList<QAnsiTextEditFormattedText>   parseText           (const QAnsiTextEditFormattedText& input);
        void                                endFormatScope      ();
        void                                setFormatScope      (const QTextCharFormat& charFormat);
        QTextCharFormat                     formatScope         () const;

    private:
        QColor                              ansiColor           (uint code);

        bool                                _previousFormatClosed = true;
        bool                                _waitingForTerminator = false;
        QString                             _alternateTerminator;
        QTextCharFormat                     _previousFormat;
        QString                             _pendingText;
};

class QAnsiTextEdit : public QPlainTextEdit {

    Q_OBJECT

    public:
        explicit QAnsiTextEdit(QWidget* parent = 0);
        explicit QAnsiTextEdit(const QString& text, QWidget* parent = 0);
       ~QAnsiTextEdit();

    private:
        void            dumpCharFormat          (QString string, QTextCharFormat format);

    public slots:
        void            setAnsiText             (const QString& text);
        void            appendAnsiText          (const QString& text);
        void            insertAnsiText          (const QString& text);

    private:
        QAnsiTextEditEscapeCodeHandler      _escapeCodeHandler;

};

