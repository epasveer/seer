# =============================================================================
# STM32 Firmware Build Environment
# =============================================================================

FROM ubuntu:22.04

ENV DEBIAN_FRONTEND=noninteractive \
    TZ=UTC \
    PATH="/opt/arm-gnu-toolchain/bin:${PATH}"

# ----------------------------------------------------------------------
# 1. Install system packages
# ----------------------------------------------------------------------
RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates curl xz-utils \
    make cmake ninja-build git \
    python3 python3-venv libncurses5 \
    dfu-util stlink-tools sudo \
    && rm -rf /var/lib/apt/lists/*

# ----------------------------------------------------------------------
# 2. Install Arm GNU Toolchain
# ----------------------------------------------------------------------
RUN curl -L -o /tmp/tc.tar.xz \
    https://developer.arm.com/-/media/Files/downloads/gnu/15.2.rel1/binrel/arm-gnu-toolchain-15.2.rel1-x86_64-arm-none-eabi.tar.xz && \
    mkdir -p /opt/arm-gnu-toolchain && \
    tar -xJf /tmp/tc.tar.xz -C /opt/arm-gnu-toolchain --strip-components=1 && \
    rm -f /tmp/tc.tar.xz

# ----------------------------------------------------------------------
# 3. Create user + permissions
# ----------------------------------------------------------------------
RUN useradd -m -s /bin/bash -u 1000 developer && \
    echo "developer ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/developer && \
    chmod 0440 /etc/sudoers.d/developer && \
    mkdir -p /workdir && chown developer:developer /workdir && \
    echo 'export PATH="/opt/arm-gnu-toolchain/bin:$PATH"' >> /home/developer/.bashrc

USER developer
WORKDIR /workdir

COPY --chown=developer:developer ./entrypoint.sh /opt/entrypoint.sh
RUN chmod +x /opt/entrypoint.sh

ENTRYPOINT ["/opt/entrypoint.sh"]
CMD ["bash"]