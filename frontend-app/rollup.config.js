
import path from 'path';

// Guide docs: https://rollupjs.org/guide/en/#rollupplugin-node-resolve
// Example: https://github.com/rollup/rollup-starter-app/blob/master/rollup.config.js

import pluginNodeResolve from '@rollup/plugin-node-resolve';
import pluginCommonjs from '@rollup/plugin-commonjs';
import pluginNodejsGlobals from 'rollup-plugin-node-globals';
import pluginBabel from '@rollup/plugin-babel';

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

const reactPlaygroundBundle = {
  input: 'src/playground/react-playground.js',
  external: [
    'react',
    'react-dom',
    'redux',
    'redux-thunk',
  ],
  output: {
    file: 'static/react-playground.js',
    format: 'es', // es, cjs, iife --- all are working
    //name: 'Rollup_reactPlayground',
  },
  plugins: [
    // Deal with the JSX (reads `babel.config.js`)
    pluginBabel({ babelHelpers: 'bundled' }),

    //// Using jspm for a minimal bundle, therefore skipping these:
    // Find React from node_modules
    // pluginNodeResolve(),
    // React is in CommonJS (i.e. without "default" member).
    // This adapts cjs modules to es6 modules:
    // pluginCommonjs(),
    // React reads process.env
    // pluginNodejsGlobals(),
  ],
};

function synonymousTsOut(input) {
  return [`ts-out/${input}`, `static/${input}`];
}

export default [
  reactPlaygroundBundle,
  roll(...synonymousTsOut('main.js')),
  roll(...synonymousTsOut('playground/initial-js-playground.js')),
];
