/**
 * \file input.h
 */
#ifndef _input_h
#define _input_h

#include <stdint.h>
#include "bool.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct Input {
    uint8_t pin;        /**< Pin associated with this input */
    bool state,         /**< Current pin state */
         last_state;    /**< Pin state last time we checked */
} Input;

void input_new(Input *input, int pin, bool pullup);
extern void input_delete(Input *);
extern void input_update(Input *);
extern bool input_went_high(Input *);
extern bool input_went_low(Input *);
extern bool input_is_high(Input *);
extern bool input_is_low(Input *);

#ifdef __cplusplus
}
#endif

#endif
