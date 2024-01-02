/**
 * \file states.h
 */

#ifndef _states_h
#define _states_h

enum STATE {
    STATE_START,        /**< Power has just been applied to mc */
    STATE_POWERWAIT0,   /**< Set powerwait timer */
    STATE_POWERWAIT1,   /**< Wait for USB signal to stabilize */
    STATE_POWERON,      /**< Assert EN */
    STATE_BOOTWAIT0,    /**< Set bootwait timer */
    STATE_BOOTWAIT1,    /**< Wait for Pi to assert BOOT or timer expiry */
    STATE_BOOT,         /**< System has booted */
    STATE_SHUTDOWN0,    /**< Set shutdown timer */
    STATE_SHUTDOWN1,    /**< Wait for Pi to de-assert BOOT or timer expiry */
    STATE_POWEROFF0,    /**< Set poweroff timer */
    STATE_POWEROFF1,    /**< Wait for timer expiry */
    STATE_POWEROFF2,    /**< Power off */
    STATE_IDLE0,        /**< Enter low-power mode */
    STATE_IDLE1,        /**< Start idle timer */
    STATE_IDLE2,        /**< Wait for power button press */
    STATE_UNMANAGED0,   /**< Do not manage power */
    STATE_UNMANAGED1,   /**< Do not manage power */
    STATE_UNMANAGED2,   /**< Do not manage power */
    STATE_QUIT          /**< Force main loop exit (debugging) */
};

#endif // _states_h
