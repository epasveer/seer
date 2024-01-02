/**
 * \file pipowerd.c
 *
 * Monitor GPIO for the SHUTDOWN signal.
 */
#define _POSIX_C_SOURCE 200809L

#include <errno.h>
#include <fcntl.h>
#include <getopt.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>

#include <linux/gpio.h>

#ifndef DEFAULT_GPIO_DEV
/** On which gpio device is our pin of interest? */
#define DEFAULT_GPIO_DEV "/dev/gpiochip0"
#endif

#ifndef DEFAULT_PIN
/** GPIO pin to monitor for the shutdown signal */
#define DEFAULT_PIN 17
#endif

#ifndef DEFAULT_SHUTDOWN_COMMAND
/** Command to run when a shutdown signal is received */
#define DEFAULT_SHUTDOWN_COMMAND "/bin/systemctl poweroff"
#endif

#define OPT_GPIO_DEV 'd'                /**< `--device|-d <device>` */
#define OPT_PIN 'p'                     /**< `--pin|-p <pin>` */
#define OPT_SHUTDOWN_COMMAND 'c'        /**< `--shutdown-command|-c <command> ` */
#define OPT_VERBOSE 'v'                 /**< `--verbose|-v` (may be specified multiple times) */
#define OPT_IGNORE_INITIAL_STATE 'i'    /**< `--ignore-initial-state|-i` */
#define OPT_HELP 'h'                    /**< `--help|-h` */

/** Valid single character options */
#define OPTSTRING "d:p:c:vih"

/** Configure options handling */
const struct option longopts[] = {
    {"gpio-device", required_argument, 0, OPT_GPIO_DEV},
    {"gpio-pin", required_argument, 0, OPT_PIN},
    {"shutdown-command", required_argument, 0, OPT_SHUTDOWN_COMMAND},
    {"ignore-initial-state", required_argument, 0, OPT_IGNORE_INITIAL_STATE},
    {"verbose", no_argument, 0, OPT_VERBOSE},
    {"help", no_argument, 0, OPT_HELP},
};

/** Holds our global configuration */
struct config {
    char *device;               /**< path to gpiochip device */

    int pin,                    /**< pin to monitor for shutdown events */
        verbose;                /**< control how verbose we are */

    bool ignore_initial_state;  /**< do not exit if shutdown pin is high at start */

    char *shutdown_command;     /**< command to run when we receive a shutdown request */
} config;

/** Initialize global configuration with default values */
void init_config() {
    config.device = DEFAULT_GPIO_DEV;
    config.pin = DEFAULT_PIN;
    config.verbose = 0;
    config.shutdown_command = DEFAULT_SHUTDOWN_COMMAND;
}

/** Display a usage message */
void usage(FILE *out) {
    fprintf(out, "pipower: usage: pipower [-d <device>] [-p <pin>] "
                 "[-c <shutdown_command> ] [-vi]\n");
}

/** Loop until we detect a shutdown request.
 *
 * First check the initial state of the shutdown pin. If a shutdown
 * request has been asserted, exit immediately unless
 * `--ignore-initial-state` was provided.  If there is no active
 * shutdown request, monitor the pin for rising edge events and exit
 * when one is received.
 */
void monitor_shutdown_pin() {
    struct gpioevent_request req;
    struct gpiohandle_data data;
    int fd;
    int ret;

    fd = open(config.device, 0);
    if (fd == -1) {
        ret = -errno;
        fprintf(stderr, "pipower: failed to open %s: %s\n",
                config.device, strerror(errno));
        exit(ret);
    }

    req.lineoffset = config.pin;
    req.handleflags = 0;
    req.eventflags = GPIOEVENT_REQUEST_RISING_EDGE;
    strcpy(req.consumer_label, "pipower-shutdown");

    // Request an file descriptor for monitor events and
    // set the line label
    ret = ioctl(fd, GPIO_GET_LINEEVENT_IOCTL, &req);
    if (ret == -1) {
        ret = -errno;
        fprintf(stderr, "pipower: failed to GET_LINEEVENT: %s\n",
                strerror(errno));
        exit(ret);
    }

    /* Get current value of pin */
    ret = ioctl(req.fd, GPIOHANDLE_GET_LINE_VALUES_IOCTL, &data);
    if (ret == -1) {
        ret = -errno;
        fprintf(stderr, "pipower: failed to GET_LINE_VALUES: %s\n",
                strerror(errno));
        exit(ret);
    }

    if (data.values[0]) {
        if (config.ignore_initial_state) {
            if (config.verbose > 0)
                fprintf(stderr, "pipower: ignoring active shutdown request\n");

        } else {
            fprintf(stderr, "pipower: shutdown request is already active\n");
            return;
        }
    }

    while (1) {
        struct gpioevent_data event;

        ret = read(req.fd, &event, sizeof(event));
        if (ret == -1) {
            if (errno == -EAGAIN) {
                continue;
            } else {
                ret = -errno;
                fprintf(stderr, "pipower: failed to read event: %s\n",
                        strerror(errno));
                exit(ret);
            }
        }

        if (ret != sizeof(event)) {
                ret = -EIO;
                fprintf(stderr, "pipower: failed to read event: %s\n",
                        strerror(errno));
                exit(ret);
        }

        // If we get this far we have successfully received a rising
        // edge event, so exit the loop and return to `main()`.
        break;
    }
}

/** Handle command line options */
void parse_args(int argc, char *argv[]) {
    int option_index = 0;
    int ch;

    while (EOF != (ch = getopt_long(argc, argv, OPTSTRING, longopts, &option_index))) {
        switch (ch) {
            case OPT_GPIO_DEV:
                config.device = strdup(optarg);
                break;

            case OPT_PIN:
                config.pin = atoi(optarg);
                if (config.pin == 0) {
                    fprintf(stderr, "pipower: invalid shutdown pin specification: %s\n", optarg);
                    exit(1);
                }
                break;

            case OPT_SHUTDOWN_COMMAND:
                config.shutdown_command = strdup(optarg);
                break;

            case OPT_IGNORE_INITIAL_STATE:
                config.ignore_initial_state = true;
                break;

            case OPT_VERBOSE:
                config.verbose++;
                break;

            case OPT_HELP:
                usage(stdout);
                exit(0);
                break;

            case '?':
                usage(stderr);
                exit(2);
                break;
        }
    }

}

int main(int argc, char *argv[]) {
    init_config();
    parse_args(argc, argv);

    if (config.verbose > 0)
	    fprintf(stderr, "pipower: starting, device=%s pin=%d\n",
                config.device, config.pin);

    monitor_shutdown_pin();

    if (config.verbose > 0)
	    fprintf(stderr, "pipower: received shutdown signal\n");
        if (config.verbose > 1)
            fprintf(stderr, "pipower: running shutdown command: %s\n",
                    config.shutdown_command);

    system(config.shutdown_command);
    return 0;
}
