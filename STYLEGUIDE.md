# Introduction

Welcome to Seer, you seasoned developers and contributors. Thanks for your efforts!

This file describes the coding style used in Seer. As well as some other design/coding tidbits that Seer follows.

Just a preface, **every** developer has their own style when writing code. I'm no different. I don't think there's any right or wrong way to style your code. But I do think one should be consistent. 

So, this is Seer's style...

## Tabs

Hopefully not a contentious area.

* Tab size set to 4 spaces.
* Use 'Tab to Spaces' mode in your editor. No physical tab characters in the source.

## Line lengths.

In this day of large monitors, I see no reason to restrict line lengths to some arbitrary small number of characters. Greater than 100 characters per line is fine with me.

## Code formatters.

Code formatters, like clang-format and the ilk, are great tools for formatting code. They may not cover all the formatting cases in this style guide, though. Some manual formatting may still be needed afterwards.

One very important point. Don't reformat the entire contents of an existing file. It will likely be rejected in the PR merge process as it makes comparing diffs **extremely** hard. Just format the area that you changed/modified.

## C++ Variable names.

Variable names should be descriptive and not overly short, and not excessively long. They should be camel case with the first character being lowercase.
```
    int    iteration     = 10;
    double dampingFactor = 0.05 + 0.05*iteration;
```
Private variables, such as private members of a C++ Class, should follow the same rule but also add the requirement the variable is prefixed with an underscore ('_')
```
    private:
        QActionGroup*  _themeStyleMenuActionGroup;
        QActionGroup*  _iconColorMenuActionGroup;
```
Static variables should be camel case with the first character being uppercase.
```
    static int Next_ID = 1;
    static std::mutex ID_mutex;
```
## C++ Struct member names.

Member names in structs should be camel case with the first character being lowercase. It's ok to prefix the name with an underscore ('_') if you want.

Here is an example. ```patternExpression``` and ```charFormat``` are the struct members.
```
    struct HighlightingRule {
        QRegularExpression patternExpression;
        QTextCharFormat    charFormat;
    };
```

## C++ Class and Struct names.

Class and struct names should be camel case with the first character being uppercase.

Here are a couple examples. ```QZoomChart``` is a class name and ```QDetachTabInfo``` is a struct name.

```
    class QZoomChart : public QChart {
        public:
            QZoomChart(QGraphicsItem* parent = nullptr, Qt::WindowFlags wFlags = {});
           ~QZoomChart();
    ...
    };


    struct QDetachTabInfo {
        QString     title;
        QWidget*    widget;
        QWidget*    placeholderWidget;
    };
```

## Getters and Setters.

C++ Getter and Setter functions should be camel case with the first letter lowercase. The Setter should start with some action verb, like 'set' or 'make'. The Getter does **not** use the 'get' prefix.
```
    void    setGdbNonStopMode  (bool flag);
    bool    gdbNonStopMode     () const;

    void    setGdbServerDebug  (bool flag);
    bool    gdbServerDebug     () const;
```

## Where to place '{}' brackets.

I put '{' at the end of the same line and not on the next line. The '}' on a new line. Here are some examples for functions, loops, and 'if' statements.
```
    // Function
    int age (int years, int months, int days) {
        return years*365 + months*31 + days; // Rough number of days.
    }

    // Loops
    for (int i=0; i<30; i++) {
    }

    while (i>0) {
        i--;
    }

    // If statements
    if (j < 0) {
        cout << "Less" << endl;
    }else if (j > 0) {
        cout << "More" << endl;
    }else{
        cout << "Equal" << endl;
    }
```

## Where to place '&*' reference/pointer characters.

The '&' and '\*' character indicates a variable is a pointer or a reference. I prefer them to *cuddle up* to the datatype and not to the the variable.

A simple example:
```
    int*    values;
```
Not:
```
    int     *values;
```
Declaring function parameters would look like:
```
    void  setAData (const QString& label, DataStorage* pData);
    void  setBData (const QString& label, DataStorage* pData);
```
Decalaring the function return type would look like:
```
    const QString&  aLabel () const;
    const QString&  bLabel () const;
```



## Const Correctness.

For C++, being Const Correct is an advantage. It can help detect some errors at compile time.

Const Correctness means using the 'const' keyword in certain places.

* At the end of a function declaration to tell the compiler the function doesn't change state of internal variables.
* In front of variables/objects that are passed to a function. Which means the function can't attempt to change the value of the variable/object.
* In front of the variable/object that is returned from a function. When means the return value can't be changed.
* When declaring a variable with a value and not allowing the variable to be changed afterwards.

Here are some examples.

Declare the function as Const. The 'gdbNonStopMode' function doesn't change any internal state. Can be overridden by using the 'mutable' keyword, though.
```
    bool  gdbNonStopMode () const;
```

Variables passed to a function can be specified with Const. The tells the compiler the function can't attempt to change them.
```
    void gdbSetDebugName(const QString& name, const QString& mode);
```

Declare the return value as Const. The 'gdbDebugName' function returns the value in such a way that it will detect at compile time if the code tries to change the return value of QString.
```
    const QString&  gdbDebugName () const;
```
Declaring a variable inside a function as Const means the variable is set with a value and can't be changed later.
```
    const int age = 65;
```

## Minimalized header files.

This is a style I like. Its aim is solely to keep the header file minimal, yet complete.

* Function and variable declarations only.
* Don't implement functions in the header file.
* The implementation is in the .cpp file.
* Doxygen comments/tags go in the .cpp file.

Header files then become a minimal and complete reference to your classes.

## Formatting lines into columns.

When the opportunity arises, I like to organize code into columns. It's sounds weird, I know. But, visually, I think it makes the code easier to read.

I see two main areas that benefit from this.

* Declaring functions in a header file.
* Connecting Qt signals and slots together.

Take this example of declaring functions in a header file. The list of function declarations are put into three *colums*. Return value, Function name, and Function parameters.
```
    |Return value............|  |Function name...|      |Function paramters...................|
    const QString&              aAxis                   () const;
    void                        setAAxis                (const QString& axis);
    const QString&              aLabel                  () const;
    void                        setAAddressOffset       (unsigned long offset);
    unsigned long               aAddressOffset          () const;
    void                        setAAddressStride       (unsigned long stride);
    unsigned long               aAddressStride          () const;
    unsigned long               aSize                   () const;
    unsigned long               aElementSize            () const;

    void                        setAArrayMode           (SeerArrayWidget::ArrayMode arrayMode);
    SeerArrayWidget::ArrayMode  aArrayMode              () const;
    QString                     aArrayModeString        () const;
    const QVector<double>&      aArrayValues            () const;
```
To me, that looks a whole lot better than no columns.
```
    const QString& aAxis() const;
    void setAAxis(const QString& axis);
    const QString& aLabel() const;
    void setAAddressOffset(unsigned long offset);
    unsigned long aAddressOffset() const;
    void setAAddressStride(unsigned long stride);
    unsigned long aAddressStride() const;
    unsigned long aSize()const;
    unsigned long aElementSize() const;

    void setAArrayMode(SeerArrayWidget::ArrayMode arrayMode);
    SeerArrayWidget::ArrayMode aArrayMode() const;
    QString aArrayModeString() const;
    const QVector<double>& aArrayValues() const;
```
The code to connect Qt signal and slots together can benefit as well.
```
                     |Signal parameters....................................................|        |Slot parameters.................................|
    QObject::connect(plainTextEdit,                  &QPlainTextEdit::cursorPositionChanged,        this, &SeerHexWidget::handleCursorPositionChanged);
    QObject::connect(showAsLittleEndianCheckBox,     &QCheckBox::clicked,                           this, &SeerHexWidget::handleCursorPositionChanged);
    QObject::connect(showUnsignedFloatAsHexCheckBox, &QCheckBox::clicked,                           this, &SeerHexWidget::handleCursorPositionChanged);
    QObject::connect(this,                           &SeerHexWidget::byteOffsetChanged,             this, &SeerHexWidget::handleByteOffsetChanged);
```
Looks much better than with no columns.
```
    QObject::connect(plainTextEdit, &QPlainTextEdit::cursorPositionChanged, this, &SeerHexWidget::handleCursorPositionChanged);
    QObject::connect(showAsLittleEndianCheckBox, &QCheckBox::clicked, this, &SeerHexWidget::handleCursorPositionChanged);
    QObject::connect(showUnsignedFloatAsHexCheckBox, &QCheckBox::clicked, this, &SeerHexWidget::handleCursorPositionChanged);
    QObject::connect(this, &SeerHexWidget::byteOffsetChanged, this, &SeerHexWidget::handleByteOffsetChanged);
```

## That's about it.
Again, **many thanks** for your efforts!

