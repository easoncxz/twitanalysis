'use strict';

import * as Redux from 'redux';
import ReactDOM from 'react-dom';
import { createHashHistory } from 'history';

import { Model, Msg, reducer, parseLocation } from './core';
import { routing } from './views';
import * as routes from './history-redux';

type ModelAndRouter = Redux.CombinedState<{
  model: Model;
  route: routes.Model;
}>;

type MsgOrRouter = Msg | routes.Msg;

const hist = createHashHistory();
const store: Redux.Store<ModelAndRouter, MsgOrRouter> = Redux.createStore(
  Redux.combineReducers({
    model: reducer,
    route: routes.reduce(hist.location),
  }),
);

hist.listen(routes.listener(store.dispatch));

const mountpoint = document.getElementById('react-mountpoint');
store.subscribe(() => {
  ReactDOM.render(
    routing(parseLocation(store.getState().route.location)),
    mountpoint,
  );
});

if (typeof window === 'object') {
  (window as any).hist = hist; //eslint-disable-line @typescript-eslint/no-explicit-any

  window.addEventListener('load', () => {
    store.dispatch({ type: 'noop' });
  });
}
