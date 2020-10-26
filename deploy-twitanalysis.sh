#!/bin/bash

# This script is intended to be run from the home directory on the Vultr box,
# by being piped into `ssh` in a convoluted way, like this:
#
#    $ ssh vultr-syd-xs.easoncxz.com 'bash -s https://github.com/easoncxz/twitanalysis/releases/download/bd08e62/twitanalysis-0.1.0.0-1.0.0-bd08e62-x86_64-Linux.bdist.tar.gz' < deploy-twitanalysis.sh

if [ $# -lt 1 ]; then
    echo "$0 BDIST_TARBALL_URL"
    exit 1
fi

set -e
set -x

tarball_url="$1"
tarball_basename="$(basename $tarball_url)"

pushd twitanalysis-live/versions/
    if [ ! -f "$tarball_basename" ]; then
        curl -LO "$tarball_url"
    fi
popd

pushd twitanalysis-live/
    rm -rf current/
    mkdir current/
    pushd current/
        current_tarball=twitanalysis.current.tar.gz
        ln -s ../versions/"$tarball_basename" "$current_tarball"
        tar -xzf "$current_tarball"
        echo "$tarball_basename" > version
    popd
    sudo systemctl restart twitanalysis
popd
