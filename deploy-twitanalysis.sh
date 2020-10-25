#!/bin/bash

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

killall twitanalysis-exe || true
pushd current
./bin/twitanalysis-exe >> ../twitanalysis.log & echo $! > ../twitanalysis.pid
popd

echo "$tarball_basename" > twitanalysis.version
popd

echo "$tarball_basename"
