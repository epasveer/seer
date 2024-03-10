/**
 * \file button.c
 *
 * Simple debouncing routine from
 * <https://hackaday.com/2015/12/10/embed-with-elliot-debounce-your-noisy-buttons-part-ii/>
 */

#include "bool.h"
#include <stdint.h>
#include <stdlib.h>
#include <avr/io.h>

#include "button.h"
#include "millis.h"

#define BUTTON_RELEASED 0b00000111  /**< History pattern when a button is released */
#define BUTTON_PRESSED  0b11000000  /**< History pattern when a button is pressed */
#define BUTTON_MASK     0b11000111  /**< Mask out noise in the button history */
#define BUTTON_UP       0b11111111  /**< History pattern when button is up */
#define BUTTON_DOWN     0b00000000  /**< History pattern when button is down */

/** Create a new Button object. */
void button_new(Button *button, uint8_t pin, uint8_t poll_freq) {
    button->pin = pin;
    button->history = BUTTON_UP;
    button->poll_freq = poll_freq;
    button->last_poll = 0;

    // ensure pin is an input
    DDRB &= ~(1<<pin);

    // set pull-up
    PORTB |= 1<<pin;
}

/** Push current button state onto history. */
void button_update(Button *button) {
    unsigned long now = millis();

    if (now - button->last_poll >= button->poll_freq) {
        button->history = button->history << 1;
        button->history |= (PINB & (1<<button->pin))>>button->pin;
        button->last_poll = now;
    }
}

/** Return true if button has been pressed. */
bool button_is_pressed(Button *button) {
    bool pressed = 0;

    if ((button->history & BUTTON_MASK) == BUTTON_PRESSED) {
        pressed = 1;
        button->history = BUTTON_DOWN;
    }

    return pressed;
}

/** Return true if button has been released. */
bool button_is_released(Button *button) {
    bool released = 0;

    if ((button->history & BUTTON_MASK) == BUTTON_RELEASED) {
        released = 1;
        button->history = BUTTON_UP;
    }

    return released;
}

/** Return true if button is up */
bool button_is_up(Button *button) {
    return (button->history == BUTTON_UP);
}

/** Return true if button is down */
bool button_is_down(Button *button) {
    return (button->history == BUTTON_DOWN);
}
