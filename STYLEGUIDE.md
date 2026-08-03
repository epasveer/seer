# Introduction

Welcome to Seer, you seasoned developer and contributor!

This file describes the coding style used in Seer. As well as other design tidbits that Seer follows.

Just a preface, **every** developer has their own style when writing code. I'm no different. I don't think there's any right or wrong way to style your code. But I do think one should be consistent. 

So, this is Seer's style...

## Tabs

Hopefully not a contentious area.

* Tab size set to 4 spaces.
* Use Tab to Spaces mode in your editor. No physical tab characters.

## C++ Variable names.

Variable names should be descriptive and not overly short, yet not excessively long. They should be camel case with the first character being lowercase.
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
## C++ Function names.
## C++ Class names.
## Where to place '{}' brackets.
## Line length.
## Where to place '&*' reference/pointer characters.
## Formatting lines into columns.



