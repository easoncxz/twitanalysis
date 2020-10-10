// @ts-ignore
module.exports = {
  'env': {
    'browser': true,
    'es2020': true
  },
  'extends': [
    'eslint:recommended',
    'plugin:react/recommended',
  ],
  'parserOptions': {
    'ecmaFeatures': {
      'jsx': true
    },
    'ecmaVersion': 11,
    'sourceType': 'module'
  },
  'plugins': [
    'react',
  ],
  'rules': {
    'indent': [
      'error',
      2
    ],
    'linebreak-style': [
      'error',
      'unix'
    ],
    'quotes': [
      'error',
      'single'
    ],
    'semi': [
      'error',
      'always'
    ],
    'no-unused-vars': [
      'error',
      {
        'varsIgnorePattern': '^_',
        'argsIgnorePattern': '^_',
      },
    ],
    'react/prop-types': 'off',
  },
  'overrides': [
    {
      'files': ['**/*.ts', '**/*.tsx'],
      'excludedFiles': [
        '.eslintrc.js',
      ],
      'extends': [
        'eslint:recommended',
        'plugin:react/recommended',
        'plugin:@typescript-eslint/recommended',

        // https://www.robertcooper.me/using-eslint-and-prettier-in-a-typescript-project
        'prettier/@typescript-eslint',
        'plugin:prettier/recommended',
      ],
      'parser': '@typescript-eslint/parser',
      'plugins' : [
        '@typescript-eslint',
      ],
      'rules': {
        '@typescript-eslint/ban-types': 'off',
        '@typescript-eslint/no-unused-vars': [
          'error',
          {
            'varsIgnorePattern': '^_',
            'argsIgnorePattern': '^_',
          },
        ],
      }
    },
  ],
};
