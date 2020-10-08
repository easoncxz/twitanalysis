
import path from 'path';

// Guide docs: https://rollupjs.org/guide/en/#rollupplugin-node-resolve
// Example: https://github.com/rollup/rollup-starter-app/blob/master/rollup.config.js

import pluginNodeResolve from '@rollup/plugin-node-resolve';
import pluginCommonjs from '@rollup/plugin-commonjs';
import pluginNodejsGlobals from 'rollup-plugin-node-globals';

void pluginNodejsGlobals;

function roll(input, output) {
  return {
    input,
    output: {
      file: output,
      format: 'iife',
      name: `Rollup_${path.basename(input, 'js')}`,
    },
    plugins: [
      pluginNodeResolve(),
      pluginCommonjs(),
      pluginNodejsGlobals(),
    ],
  };
}

function synonymousTsOut(input) {
  return [`ts-out/${input}`, `static/${input}`];
}

export default [
  roll(...synonymousTsOut('initial-js-playground.js')),
  roll(...synonymousTsOut('main.js')),
];
