/**
 * \file pipower.c
 *
 * Raspberry Pi/Powerboost 1000c/ATtiny85  power controller
 */

#include <stdint.h>
#include <avr/io.h>
#include <avr/interrupt.h>
#include <avr/sleep.h>

#include "bool.h"
#include "button.h"
#include "millis.h"
#include "input.h"
#include "pins.h"
#include "states.h"

/** \defgroup Button Power button
 * @{
 */
#define LONG_PRESS_DURATION 2000    /**< Length of long press */
#define BUTTON_NORMAL 0             /**< Process button events normally */
#define BUTTON_IGNORE 1             /**< Power button must be released */
/** @} */

/** \defgroup Timers Timers
 * @{
 */
#define ONE_SECOND 1000
#define TIMER_BUTTON 10                     /**< Period for reading button state */
#ifndef TIMER_POWERWAIT
#define TIMER_POWERWAIT (1 * ONE_SECOND)    /**< How long to wait for USB to stabilize */
#endif

#ifndef TIMER_BOOTWAIT
#define TIMER_BOOTWAIT (30 * ONE_SECOND)    /**< How long to wait for boot */
#endif

#ifndef TIMER_SHUTDOWN
#define TIMER_SHUTDOWN (30 * ONE_SECOND)    /**< How long to wait for shutdown */
#endif

#ifndef TIMER_POWEROFF
#define TIMER_POWEROFF (30 * ONE_SECOND)    /**< How long to wait for power off */
#endif

#ifndef TIMER_IDLE
#define TIMER_IDLE (5 * ONE_SECOND)         /**< How long to wait before returning to SLEEP_PWRDOWN mode */
#endif

/** @} */

unsigned long now;  /**< Set to the current value of `millis()` on each loop iteration. */
unsigned long timer_start = 0;  /**< Generic timer used in state transitions */

/** \addtogroup Button
 * @{
 */

Button power_button;                       /**< Power button */
uint8_t power_button_state = BUTTON_NORMAL; /**< Power button current state */
unsigned long time_pressed;                 /**< Current press duration */
/** @} */

/** Current run state. */
enum STATE state = STATE_START;

/** Allow debugger to trigger exit from main loop */
bool quit = false;

Input usb;     /**< USB signal from PowerBoost */
Input boot;    /**< BOOT signal from Raspberry Pi */

/** Trigger wake from power-down mode on pin change interrupt.
 *
 * We rely on the pin change interrupt to wake from `SLEEP_PWRDOWN` mode, 
 * but we do not otherwise need to handle the interrupt, so this is a
 * do-nothing routine.
 */
EMPTY_INTERRUPT(PCINT0_vect);

/** Run once when mc boots. */
void setup() {
    // PIN_EN and PIN_SHUTDOWN are outputs
    DDRB = 1<<PIN_EN | 1<<PIN_SHUTDOWN;

    // Enable pin change interrupts on pins PIN_POWER and PIN_USB
    // (but note that we do not enable pin change interrupts in
    // GIMSK here).
    PCMSK |= 1<<(PIN_POWER) | 1<<(PIN_USB);

    button_new(&power_button, PIN_POWER, TIMER_BUTTON);
    input_new(&usb, PIN_USB, false);
    input_new(&boot, PIN_BOOT, true);

    init_millis();
}

/** Enable pin change interrupts in GIMS */
void enable_pcie() {
    GIMSK |= 1<<PCIE;
}

/** Disable pin change interrupts in GIMS */
void disable_pcie() {
    GIMSK &= ~(1<<PCIE);
}


/** Runs periodically */
void loop() {
    bool long_press = false,
         short_press = false;

    now = millis();
    input_update(&usb);
    input_update(&boot);
    button_update(&power_button);

    // Detect short and long presses
    if (power_button_state == BUTTON_IGNORE) {
        if (button_is_released(&power_button)) {
            power_button_state = BUTTON_NORMAL;
        }
    } else if (button_is_pressed(&power_button)) {
        time_pressed = now;
    } else if (button_is_released(&power_button)) {
        short_press = true;
    } else if (button_is_down(&power_button)) {
        unsigned long delta = now - time_pressed;
        if (delta > LONG_PRESS_DURATION) {
            long_press = true;
            power_button_state = BUTTON_IGNORE;
        }
    }

    // At any point, a long press will force the power off.
    if (long_press) {
        if (state == STATE_IDLE2) {
            state = STATE_UNMANAGED0;
        } else {
            state = STATE_POWEROFF2;
        }
    }

    switch(state) {
        case STATE_START:
            if (input_is_high(&usb)) {
                // USB goes high briefly when the microcontroller starts
                // up. Wait a second for it to stabilize before we try to
                // boot.
                state = STATE_POWERWAIT0;
            } else {
                state = STATE_POWEROFF2;
            }
            break;

        case STATE_POWERWAIT0:
            // start powerwait timer
            timer_start = now;
            state = STATE_POWERWAIT1;
            break;

        case STATE_POWERWAIT1:
            if (input_is_low(&usb)) {
                state = STATE_POWEROFF2;
            } else if (now - timer_start >= TIMER_POWERWAIT) {
                state = STATE_POWERON;
            }
            break;

        case STATE_POWERON:
            // Assert EN
            PORTB |= 1<<PIN_EN;
            state = STATE_BOOTWAIT0;
            break;

        case STATE_BOOTWAIT0:
            // Start bootwait timer
            timer_start = now;
            state = STATE_BOOTWAIT1;
            break;

        case STATE_BOOTWAIT1:
            // Wait for Pi to assert BOOT or timeout
            if (now - timer_start >= TIMER_BOOTWAIT) {
                state = STATE_POWEROFF2;
            } else if (input_is_low(&boot)) {
                state = STATE_BOOT;
            }
            break;

        case STATE_BOOT:
            // Wait for power button or Pi to de-assert BOOT
            if (short_press || input_went_low(&usb)) {
                state = STATE_SHUTDOWN0;
            } else if (input_is_high(&boot)) {
                state = STATE_POWEROFF0;
            }
            break;

        case STATE_SHUTDOWN0:
            // Assert SHUTDOWN
            PORTB |= 1<<PIN_SHUTDOWN;
            timer_start = now;
            state = STATE_SHUTDOWN1;
            break;

        case STATE_SHUTDOWN1:
            // Wait for Pi to de-assert BOOT or timeout
            if (now - timer_start >= TIMER_SHUTDOWN) {
                state = STATE_POWEROFF0;
            } else if (input_is_high(&boot)) {
                state = STATE_POWEROFF0;
            }
            break;

        case STATE_POWEROFF0:
            // Start poweroff timer
            PORTB &= ~(1<<PIN_SHUTDOWN);
            timer_start = now;
            state = STATE_POWEROFF1;
            break;

        case STATE_POWEROFF1:
            // Wait for poweroff timer to expire.
            if (now - timer_start >= TIMER_POWEROFF) {
                state = STATE_POWEROFF2;
            } else if (input_is_low(&boot)) {
                // Pi has re-asserted BOOT
                state = STATE_BOOT;
            }
            break;

        case STATE_POWEROFF2:
            // De-assert EN and SHUTDOWN
            PORTB &= ~(1<<PIN_EN);
            PORTB &= ~(1<<PIN_SHUTDOWN);
            state = STATE_IDLE0;
            break;

        case STATE_IDLE0:
            // Enter low power mode.
            enable_pcie();
            set_sleep_mode(SLEEP_MODE_PWR_DOWN);
            sleep_mode();
            disable_pcie();
            state = STATE_IDLE1;
            break;

        case STATE_IDLE1:
            timer_start = now;
            state = STATE_IDLE2;
            break;

        case STATE_IDLE2:
            if (now - timer_start >= TIMER_IDLE) {
                // return to low power mode if nothing to do
                state = STATE_IDLE0;
            } else if (short_press && input_is_high(&usb)) {
                // power on if power button pressed and power is available
                state = STATE_POWERON;
            } else if (input_went_high(&usb)) {
                // power on if power is restored
                state = STATE_POWERON;
            }
            break;

        case STATE_UNMANAGED0:
            // Enter low power mode.
            enable_pcie();
            set_sleep_mode(SLEEP_MODE_PWR_DOWN);
            sleep_mode();
            disable_pcie();
            state = STATE_UNMANAGED1;
            break;

        case STATE_UNMANAGED1:
            timer_start = now;
            state = STATE_UNMANAGED2;
            break;

        case STATE_UNMANAGED2:
            if (now - timer_start >= TIMER_IDLE) {
                // return to low power mode if nothing to do
                state = STATE_UNMANAGED0;
            } else if (short_press) {
                // short press in this state will toggle power
                PORTB ^= 1<<PIN_EN;
            }
            break;

        case STATE_QUIT:
            /* This state is only used during debugging to force a main loop
             * exit. */
            return;
    }
}

int main() {
    setup();

    while (state != STATE_QUIT) {
        loop();
    }
}
