# Author: Cortex-Debug extension
# Cortex-Debug extension calls this function during initialization. You can copy this
# file, modify it and specifyy it as one of the config files supplied in launch.json
# preferably at the beginning.
#
# Note that this file simply defines a function for use later when it is time to configure
# for SWO.
#
set USE_SWO 0
proc CDSWOConfigure { CDCPUFreqHz CDSWOFreqHz CDSWOOutput } {    
    # We don't create/configure the entire TPIU which requires advanced knowledge of the device
    # like which DAP/AP ports to use, what their bases addresses are, etc. That should already
    # be done by the config files from the Silicon Vendor
    catch {tpiu init}; # we are allowed to call this multiple times. So, call it just in case
    set tipu_names [tpiu names]
    if { [llength $tipu_names] == 0 } {
        puts stderr "[info script]: Error: Could not find TPIU/SWO names. Perhaps it hasn't been created?"
    } else {
        set mytpiu [lindex $tipu_names 0]
        # We don't create/configure the entire TPIU which requires advanced knowledge of the device
        # like which DAP/AP ports to use, what their bases addresses are, etc. That should already
        # be done by the config files from the Silicon Vendor
        puts "[info script]: $mytpiu configure -protocol uart -output $CDSWOOutput -traceclk $CDCPUFreqHz -pin-freq $CDSWOFreqHz"
        $mytpiu configure -protocol uart -output $CDSWOOutput -traceclk $CDCPUFreqHz -pin-freq $CDSWOFreqHz
        $mytpiu enable
    }
}

#
# The following function may not work in a multi-core setup. You may want to overide this function
# to enable RTOS detection for each core appropriately. This function must be called before `init` and
# after all the targets are created.
#
proc CDRTOSConfigure { rtos } {
    set target [target current]
    if { $target != "" } {
        puts "[info script]: $target configure -rtos $rtos"
        $target configure -rtos $rtos
    } else {
        # Maybe this function was called too early?
        puts stderr "[info script]: Error: No current target. Could not configure target for RTOS"
    }
}

#
# CDLiveWatchSetup
#    This function must be called before the init is called and after all the targets are created. You can create
#    a custom version of this function (even empty) if you already setup the gdb-max-connections elsewhere
#
#    We increment all gdb-max-connections by one if it is already a non-zero. Note that if it was already set to -1,
#    we leave it alone as it means unlimited connections
#
proc CDLiveWatchSetup {} {
    try {
        foreach tgt [target names] {
            set nConn [$tgt cget -gdb-max-connections]
            if { $nConn > 0 } {
                incr nConn
                $tgt configure -gdb-max-connections $nConn
                puts "[info script]: Info: Setting gdb-max-connections for target '$tgt' to $nConn"
            }
        }
    } on error {} {
        puts stderr "[info script]: Error: Failed to increase gdb-max-connections for current target. Live variables will not work"
    }
}
