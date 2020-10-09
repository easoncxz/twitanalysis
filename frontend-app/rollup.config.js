
import path from 'path';

// Guide docs: https://rollupjs.org/guide/en/#rollupplugin-node-resolve
// Example: https://github.com/rollup/rollup-starter-app/blob/master/rollup.config.js

import pluginNodeResolve from '@rollup/plugin-node-resolve';
import pluginCommonjs from '@rollup/plugin-commonjs';
import pluginNodejsGlobals from 'rollup-plugin-node-globals';
import pluginBabel from '@rollup/plugin-babel';

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

const reactPlaygroundBundle = {
  input: 'src/react-playground.js',
  output: {
    file: 'static/react-playground.js',
    format: 'es',
  },
  plugins: [
    pluginBabel({
      babelHelpers: 'bundled',
      presets: [
        '@babel/preset-env',
        '@babel/preset-react',
      ],
    }),
  ],
};
export default [
  reactPlaygroundBundle,
  roll(...synonymousTsOut('initial-js-playground.js')),
  roll(...synonymousTsOut('main.js')),
];
