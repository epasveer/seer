/**
 * \file input.c
 */

#include "bool.h"
#include <stdint.h>
#include <stdlib.h>
#include <avr/io.h>

#include "input.h"

/** Create a new Input object */
void input_new(Input *input, int pin, bool pullup) {
    input->pin = pin;
    input->state = false;
    input->last_state = false;

    // Ensure pin is an input.
    DDRB &= ~(1<<pin);

    // Set pullup if requested
    if (pullup) {
	    PORTB |= 1<<pin;
    }

    input_update(input);
    input->last_state = input->state;
}

/** Read the state of the pin */
void input_update(Input *input) {
    input->state = (PINB & (1<<input->pin))?1:0;
}

/** Return true if the pin state has gone high.
 *
 * This checks if the pin state has changed since a previous call to
 * `went_high()` or `went_low()`.
 */
bool input_went_high(Input *input) {
    bool ret = (!input->last_state && input->state);
    input->last_state = input->state;
    return ret;
}

/** Return true if the pin state has gone low.
 *
 * This checks if the pin state has changed since a previous call to
 * `went_high()` or `went_low()`.
 */
bool input_went_low(Input *input) {
    bool ret = (input->last_state && !input->state);
    input->last_state = input->state;
    return ret;
}

/** Return true if the pin is high */
bool input_is_high(Input *input) {
    return input->state;
}

/** Return true if the pin is low */
bool input_is_low(Input *input) {
    return !input->state;
}
