.PHONY: build_docker docker buildBBB install_docker

DEVICE:=

default:
	/bin/bash build_Rpi4_owrt.sh

install_docker:
	bash install_docker.sh

build_docker:
	docker build -t docker-bbb Docker

docker:
	docker run --rm -it --user $(id -u):$(id -g) \
	--name "${USER}_bbb" \
	--mount type=bind,source="${HOME}/.bash_history",target=/home/qca-user/.bash_history \
	--mount type=bind,source="${HOME}/.bashrc",target=/home/qca-user/.bashrc \
	--mount type=bind,source="${HOME}/.gitconfig",target=/home/qca-user/.gitconfig \
	--mount type=bind,source="${HOME}/.ssh",target=/home/qca-user/.ssh \
	--mount type=bind,source="${PWD}",target=/workdir \
	--workdir=/workdir \
	docker-bbb
