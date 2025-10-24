FROM ubuntu:22.04
ARG USER_NAME_INTERNAL=developer

# Original Dockerfile switched to the unprivileged user 'yoctouser' at this
# point. Switch back to the root user so we can install some additionnal
# packages, and switch back.

USER root
ENV TZ=Asia/Kolkata \
    DEBIAN_FRONTEND=noninteractive


# install system-wide deps for building
# jq is only needed for the versioning system, not required for the build part
RUN apt-get -y update && \
    apt-get -y install xxd \
                git cpio lz4 bc lsb-release man-db libmpc-dev zstd \
                rsync opam ocaml ocaml-nox ocaml-findlib \
                coreutils openvswitch-common openvswitch-switch \
                build-essential clang gcc-multilib \
                dos2unix \
                zip \
                curl \
                libncurses5-dev \
                jq \
                patch \
                bzip2 \
                flex \
                make \
                gettext \
                pkg-config \
                unzip \
                zlib1g-dev \
                libc6-dev \
                subversion \
                libncurses5-dev \
                gawk \
                sharutils curl \
                libxml-parser-perl \
                ocaml-nox \
                ocaml \
                gcc g++ \
                ocaml-findlib \
                libfdt-dev \
                device-tree-compiler \
                u-boot-tools \
                libssl-dev \
                libavro23 \
                avro-bin \
                libavro-dev \
                mtd-utils \
                libc-ares2 \
                libc-ares-dev \
                bison \
                openvswitch-switch \
                uuid-dev \
                libblkid-dev \
                libzstd-dev libbz2-dev \
                libncurses-dev \
                libreadline-dev \
                libgmp-dev \
                g++-multilib sudo \
                python3-setuptools \
                swig wget bash-completion \
                libffi-dev python3-dev binutils \
                lzop && \
# Replace git protocol with https
    git config --system url.https://github.com/.insteadOf git://github.com/ && \
# Best practices in case of manual git use
    git config --system pull.rebase true

# Add user with known password
RUN useradd -ms /bin/bash $USER_NAME_INTERNAL && \
    echo "${USER_NAME_INTERNAL}:${USER_NAME_INTERNAL}" | chpasswd && \
    adduser ${USER_NAME_INTERNAL} sudo && \
    echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers

RUN apt-get update && apt-get install -y \
    gcc-aarch64-linux-gnu g++-aarch64-linux-gnu \
    make cmake pkg-config \
    && rm -rf /var/lib/apt/lists/*

RUN apt-get -y update && \
    apt-get -y install libgnutls28-dev python3-pyelftools systemd systemd-container
# Add user with known password
RUN echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers

ENV GIT_CONFIG=/workdir/.gitconfig
RUN apt-get update && apt-get install -y locales gtk-doc-tools texinfo && \
    echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen && \
    locale-gen en_US.UTF-8 && \
    update-locale LANG=en_US.UTF-8

RUN apt-get update && apt-get install -y chrpath diffstat xterm tmux nasm

RUN apt-get update && apt-get install -y iasl

ENV LANG=en_US.UTF-8 \
    LC_ALL=en_US.UTF-8


USER developer
COPY ./entrypoint.sh /opt
ENTRYPOINT ["/opt/entrypoint.sh"]
CMD ["bash"]
