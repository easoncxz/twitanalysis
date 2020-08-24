
// Guide docs: https://rollupjs.org/guide/en/#rollupplugin-node-resolve
// Example: https://github.com/rollup/rollup-starter-app/blob/master/rollup.config.js

import resolve from '@rollup/plugin-node-resolve';
import commonjs from '@rollup/plugin-commonjs';
import nodejsBuiltins from 'rollup-plugin-node-builtins';
import nodejsGlobals from 'rollup-plugin-node-globals';

void nodejsBuiltins, nodejsGlobals;

function roll(input, output) {
  return {
    input,
    output: {
      file: output,
      format: 'es'
    },
    plugins: [
      resolve(),
      commonjs(),
      //nodejsBuiltins(),
      nodejsGlobals(),
    ],
  };
}

function synonymousTs(input) {
  return [`ts-out/${input}`, `static/${input}`];
}

export default [
  roll(...synonymousTs('main.js')),
];
