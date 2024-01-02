/**
 * \file pins.h
 *
 * Pin defintitions.
 */

#ifndef _pins_h
#define _pins_h

#include <avr/io.h>

enum PINS {
    PIN_POWER=PB0,       /**< [INPUT] Power button */
    PIN_USB,             /**< [INPUT] USB signal from Powerboost */
    PIN_EN,              /**< [OUTPUT] EN to Powerboost */
    PIN_SHUTDOWN,        /**< [OUTPUT] SHUTDOWN signal to Pi */
    PIN_BOOT             /**< [INPUT] BOOT signal from Pi */
};

#endif // _pins_h
