#!/usr/bin/env bash

# Intended to be run to check whether tsc, node, ts-node are all
# playing along nice with each other.

set -e
set -x

if [ -d ts-out ]; then
  rm -rfv ts-out
fi

yarn ts-node src/nodejs/format-curl-command.ts

yarn ts-node <<EOF
import { formatCurlCommand } from './src/nodejs/format-curl-command';
console.log(formatCurlCommand('http://google.com', {body: ''}));
EOF

# Now compile:
yarn build

node ts-out/nodejs/format-curl-command.js

node <<EOF
const oc = require('./ts-out/nodejs/format-curl-command');
console.log(oc.formatCurlCommand('http://google.com', {body: ''}));
EOF

# Extra, actually unreasonable test:
#   - run the bundle for the browser, in nodejs
node static/main.js

echo "All ok"
