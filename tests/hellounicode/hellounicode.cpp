#include <iostream>
#include <stdlib.h>

int main (int argc, char* argv[]) {

    std::wstring   w_redrum   = { L"All work and no play makes Jack a dull boy.\n" };

    std::string    u8_redrum  = { u8"All work and no play makes Jack a dull boy.\n" };

    std::u16string u16_redrum = { u"All work and no play makes Jack a dull boy.\n" };

    std::u32string u32_redrum = { U"All work and no play makes Jack a dull boy.\n" };

    std::u16string u16_hungry = { u"nǐ chīle ma.\n" };
    std::u16string u16_chem   = { u"3(NH₄)₂S + Sb₂S₅ → 6NH₄⁺ + 2SbS₄³⁺\n" };

    // Do something with the strings so they don't get optimized out.
    std::cout << w_redrum.length()   << std::endl;
    std::cout << u8_redrum.length()  << std::endl;
    std::cout << u16_redrum.length() << std::endl;
    std::cout << u32_redrum.length() << std::endl;
    std::cout << u16_hungry.length() << std::endl;
    std::cout << u16_chem.length()   << std::endl;

    const wchar_t*  w_ptr      = w_redrum.data();
    const char*     u8_ptr     = u8_redrum.data();
    const char16_t* u16_ptr    = u16_redrum.data();
    const char32_t* u32_ptr    = u32_redrum.data();
    const char16_t* hungry_ptr = u16_hungry.data();
    const char16_t* chem_ptr   = u16_chem.data();

    return 0;
}

