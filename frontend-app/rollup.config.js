
import path from 'path';

// Guide docs: https://rollupjs.org/guide/en/#rollupplugin-node-resolve
// Example: https://github.com/rollup/rollup-starter-app/blob/master/rollup.config.js

import pluginNodeResolve from '@rollup/plugin-node-resolve';
import pluginCommonjs from '@rollup/plugin-commonjs';
import pluginNodejsGlobals from 'rollup-plugin-node-globals';
import pluginNodejsBuiltins from 'rollup-plugin-node-builtins';
import pluginBabel from '@rollup/plugin-babel';

void pluginBabel, pluginNodejsBuiltins;

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
  input: 'babel-out/react-playground.js',
  output: {
    file: 'static/react-playground.js',
    format: 'iife',
    name: 'Rollup_reactPlayground',
  },
  plugins: [
    // Deal with the JSX
    // pluginBabel({
    //   babelHelpers: 'bundled',
    //   presets: [
    //     '@babel/preset-env',
    //     '@babel/preset-react',
    //   ],
    // }),
    // Find React from node_modules
    pluginNodeResolve(),
    // React is CommonJS (i.e. without "default" member).
    // This adapts cjs modules to es6 modules:
    pluginCommonjs({
      // https://www.npmjs.com/package/@rollup/plugin-commonjs
      // Mentioned by issue comment: https://github.com/rollup/rollup-plugin-commonjs/issues/239#issuecomment-353764860
      //transformMixedEsModules: true,
    }),
    // React reads process.env
    pluginNodejsGlobals(),
    // React uses `require` somewhere
    // pluginNodejsBuiltins(),
  ],
};

export default [
  reactPlaygroundBundle,
  roll(...synonymousTsOut('initial-js-playground.js')),
  roll(...synonymousTsOut('main.js')),
];
