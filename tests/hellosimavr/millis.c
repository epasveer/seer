/**
 * \file millis.c
 *
 * Provide an analog of the Arduino `millis()` function. This implementation
 * uses `TIMER0` with a `/64` divider, which will allow it to operate with a
 * clock frequency of up to 16Mhz.
 */

#include <avr/interrupt.h>
#include <avr/io.h>
#include <util/atomic.h>

volatile unsigned long timer_millis = 0;

/** Timer interrupt service routine.
 *
 * This fires each time the timer counter (`TCNT0`) reaches the
 * target value stored in `OCR0A`.
 */
ISR(TIMER0_COMPA_vect) {
    timer_millis++;
}

/** Initialize the millis service.
 *
 * Configure timer 0 in CTC mode.
 * See the [attiny85 datasheet][1] for more information.
 *
 * [1]: https://ww1.microchip.com/downloads/en/DeviceDoc/Atmel-2586-AVR-8-bit-Microcontroller-ATtiny25-ATtiny45-ATtiny85_Datasheet.pdf
 */
void init_millis() {
    // Enable CTC mode
    TCCR0A = 1<<WGM01;

    // Select /64 scale, since this will let us operate at 16Mhz without
    // overflowing our 8-bit TCNT0 register.
    TCCR0B = 3<<CS00;

    // (F_CPU/1000) is number of clock cycles/ms, and we are using the
    // /64 scaler.
    OCR0A = (F_CPU/1000) / 64;

    // Enable timer compare interrupt
    TIMSK |= 1<<OCIE0A;

    sei();
}

/** Return milliseconds since mc boot. */
unsigned long millis() {
    unsigned long _millis;
    // Updating a 16 bit value is not an atomic operation.
    ATOMIC_BLOCK(ATOMIC_RESTORESTATE) {
        _millis = timer_millis;
    }
    
    return _millis;
}                                                
