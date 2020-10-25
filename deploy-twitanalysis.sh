#!/bin/bash

# This script is intended to be run from the home directory on the Vultr box.
# Commiting into this repo just as a way to keep track.

set -e
set -x

if [ $# -lt 1 ]; then
    echo "$0 BDIST_TARBALL_URL"
    exit 1
fi

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
tar -xzf versions/"$tarball_basename" -C current/

pushd current
pkill twitanalysis || true
( ./bin/twitanalysis-exe >> twitanalysis.log & echo $! > twitanalysis.pid )
echo "$tarball_basename" > twitanalysis.version
popd

popd

echo "$tarball_basename"
