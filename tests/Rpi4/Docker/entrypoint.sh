#!/bin/bash
 
USER_NAME=developer
GROUP_NAME=scom-group
 
# Verification that the user is not already created. (For bug in gitlab CI where the entrypoint is run multiple times)
if id "${USER_NAME}" >/dev/null 2>&1; then
    echo "user ${USER_NAME} found"
else
    # User creation
    if [ -d "/workdir" ]; then
        USER_ID=$(stat -c %u /workdir/)
        GROUP_ID=$(stat -c %g /workdir/)
 
        echo "Init ${USER_NAME} with uid: ${USER_ID} & gid; ${GROUP_ID}"
        sudo groupadd -g "${GROUP_ID}" "${GROUP_NAME}"
        sudo useradd ${USER_NAME} -u "${USER_ID}" -g "${GROUP_ID}" -ml -s /bin/bash
    else
        echo "Error: Directory /workdir does not exists. Unable to set the container UID & GID."
        sudo useradd ${USER_NAME} -ml -s /bin/bash
    fi
fi
 
# sudo usermod -G sudo ${USER_NAME}
 
if [ -d "/host" ]; then
    # shellcheck disable=SC2045
    for ITEM in $(ls -A /host/); do
        if [ -e "/home/${USER_NAME}/${ITEM}" ]; then
            echo "File $ITEM already present. Overwriting it with the one found in /host."
        fi
        sudo ln -svf "/host/${ITEM}" "/home/${USER_NAME}/${ITEM}"
        sudo chown "${USER_NAME}:${GROUP_NAME}" "/home/${USER_NAME}/${ITEM}"
    done
fi
 
sudo su "${USER_NAME}"