'use strict';

import React from 'react';
import ReactDOM from 'react-dom';
import Redux from 'redux';

type Model = {};

const init: Model = {};

type Action = { type: 'noop' };

function reducer(model: Model | undefined, action: Action): Model {
  if (!model) {
    return init;
  }
  switch (action.type) {
    case 'noop':
      return model;
    default:
      return model;
  }
}

type ActionCreators = {
  noop(): Action;
};

const actionsOf = (_dispatch: Redux.Dispatch<Action>): ActionCreators => ({
  noop() {
    return {
      type: 'noop',
    };
  },
});

const store: Redux.Store<Model, Action> = Redux.createStore(reducer, init);

function App() {
  return <p>Hello from React</p>;
}

const mountPoint = document.getElementById('react-mountpoint');
store.subscribe(() => {
  ReactDOM.render(<App />, mountPoint);
});

console.log('main.tsx here.');
if (
  typeof window === 'object' &&
  typeof window.addEventListener === 'function'
) {
  window.addEventListener('load', () =>
    store.dispatch(actionsOf(store.dispatch).noop()),
  );
}
