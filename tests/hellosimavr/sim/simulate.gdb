set pagination off
file pipower.elf
target remote :1234
load

##
## Helper functions
##

# wait for <n> milliseconds
define wait_for
    disable 1
    set $start_time = now
    tb loop if now == $start_time + $arg0
    c
    enable 1
end

# simulate a short press of the power button
define short_press
    set PINB=PINB & ~(1<<PIN_POWER)
    wait_for 100
    set PINB=PINB | 1<<PIN_POWER
    c
end

# log a message
define log
	printf "\n* %s\n", $arg0
end

# run until we reach the given state
define run_until_state
    disable 1
    tb loop if $arg0 == state
    c
    enable 1
end

##
## Execution starts here
##

# set an initial breakpoint at the start of loop() and advance the program
# to that point
b loop
c

# set up some information to display at each breakpoint
display state
display /t PORTB
display /t PINB
display

# let the code advance for 100ms
wait_for 100

# enable external power
log "setting PIN_USB"
set PINB=PINB | 1<<PIN_USB
run_until_state STATE_BOOTWAIT1
wait_for 100

# assert BOOT
log "resetting PIN_BOOT"
set PINB=PINB & ~(1<<PIN_BOOT)
run_until_state STATE_BOOT

##
## ...the pi has booted...
##
wait_for 1000

# request a shutdown by pressing the power button
log "pressing power button"
short_press
run_until_state STATE_SHUTDOWN1

# de-assert BOOT
wait_for 100
log "setting PIN_BOOT"
set PINB=PINB | 1<<PIN_BOOT

# step through state transitions until we reach
# STATE_IDLE2
run_until_state STATE_POWEROFF0
run_until_state STATE_POWEROFF1
run_until_state STATE_POWEROFF2
log "entering idle mode"
run_until_state STATE_IDLE0
run_until_state STATE_IDLE1
run_until_state STATE_IDLE2

wait_for 100

log "setting quit flag"
set state=STATE_QUIT
finish

disconnect
quit
