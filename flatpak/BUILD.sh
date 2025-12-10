#!/bin/bash

# Build the Seergdb flatpak

flatpak uninstall -y io.github.epasveer.seer
\rm -rf build-dir/ repo/ .flatpak-builder/ seer.flatpak
flatpak run org.flatpak.Builder --force-clean build-dir io.github.epasveer.seer.yml
flatpak run org.flatpak.Builder --force-clean --repo=repo build-dir io.github.epasveer.seer.yml
flatpak build-bundle repo seer.flatpak io.github.epasveer.seer master
flatpak install -y --bundle --user seer.flatpak

