# PiPower

Turn an Adafruit [PowerBoost 1000c][] into a UPS with the help of an [ATtiny85][] microcontroller.

[powerboost 1000c]: https://www.adafruit.com/product/2465
[attiny85]: https://www.microchip.com/wwwproducts/en/ATtiny85

## Caveats

The PowerBoost 1000c can only provide about 1.1A, which may not be sufficient to power a Raspberry Pi 3(+).  According to the [FAQ][], the Pi 3 may draw up to 1.2A under load. The typical current draw is listed as around 500mA, which would be fine.

[FAQ]: https://www.raspberrypi.org/documentation/faqs/#pi-power

## Theory of operation

PiPower is designed to ensure that your Pi will shut down cleanly when it is disconnected from its primary power source.

PiPower runs on an ATtiny85 microcontroller and acts as an intermediary between your Raspberry Pi and an Adafruit PowerBoost 1000c.  When the microcontroller (mc) boots, it checks to see if an external power source is available to the PowerBoost.  If so, it sets the `EN` line and the PowerBoost starts to supply power to the Pi.

If no external power is available, the mc enters a low power sleep mode until external power is applied.  At this point, it will boot the Pi.

When the Pi boots, it must signal to the mc that it has booted successfully by bringing the `BOOT` line low. If the mc does not receive this signal within 30 seconds of providing power, it will turn off the power and return to the low power mode.

If external power is lost while the Pi is running, or if you press the power button while the Pi is running, the mc will send the `SHUTDOWN` signal to the Pi.  It will then wait up to 30 seconds for the Pi to shut down.  The Pi can signal a clean shutdown by setting the `BOOT` line high.  Once the shutdown is complete (or if 30 seconds pass), the mc will remove power from the Pi and return to low power mode.

## Installing pipower on your attiny85

Run `make` to build the executable:

    make

Run `make flash` to upload the image to your attiny85:

    make flash

The `Makefile` assumes you are using an Arduino UNO as your ISP. If you're using something else you will need to update the Makefile appropriately.

## Pins

- `PB0` - Momentary Power Button
- `PB1` - `USB` line from PowerBoost
- `PB2` - `ENABLE` line to PowerBoost
- `PB3` - `BOOT` signal from Raspberry Pi (default `GPIO4`)
- `PB4` - `SHUTDOWN` signal to Raspberry Pi (default `GPIO17`)
- `VCC` - `5v Power` connected to the VS output from the PowerBoost    

## Installing on your Raspberry Pi

The `pipowerd` directory contains the components that need to be installed on your Raspberry Pi.  Clone the repository onto your Pi, cd into the `pipowerd` directory, and run:

    make

This will build the `pipowerd` executable, which is a daemon that will monitor a GPIO pin for the shutdown signal from `pipower`. When it receives the shutdown signal it runs `systemctl poweroff`.

To install `pipowerd` and the associated `systemd` units, run:

    make install

To enable the new services, run:

    make activate

### systemd units

The `Makefile` will install the following systemd units:

- `pipower-boot.service`

  Asserts the `BOOT` signal on `PIN_BOOT` (default `GPIO4`) when the Pi boots and de-asserts it on shutdown.

- `pipowerd.service`

  Launches the `pipowerd` daemon to monitor for shutdown signals on `PIN_SHUTDOWN`.

You can configure these services by creating the file `/etc/default/pipower`, which may set one or more of the following variables:

- `PIN_POWER` - BCM GPIO on which to assert the `BOOT` signal.
- `PIN_SHUTDOWN` - BCM GPIO on which to watch for the `SHUTDOWN` signal

## See also

- My blog post on this project at <https://blog.oddbit.com/post/2019-01-19-pipower-a-raspberry-pi-ups/>
- Doxygen documentation for this project is online at <http://oddbit.com/pipower>
- [PowerBoost 1000c data sheets/pinouts/etc](https://learn.adafruit.com/adafruit-powerboost-1000c-load-share-usb-charge-boost/downloads)
- [ATtiny85 data sheet](https://ww1.microchip.com/downloads/en/DeviceDoc/Atmel-2586-AVR-8-bit-Microcontroller-ATtiny25-ATtiny45-ATtiny85_Datasheet.pdf)

## License

PiPower, a UPS for your Raspberry Pi  
Copyright (C) 2019 Lars Kellogg-Stedman <lars@oddbit.com>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
