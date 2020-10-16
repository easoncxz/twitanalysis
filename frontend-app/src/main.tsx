'use strict';

import * as ConnRouter from 'connected-react-router';
import * as ReactRedux from 'react-redux';
import * as Redux from 'redux';
import React from 'react';
import ReactDOM from 'react-dom';
import { ConnectedRouter } from 'connected-react-router';
import { createHashHistory } from 'history';

import { Model, Msg, parsePage, reducer } from './core';
import { routing } from './views';

type ModelAndRouter = Redux.CombinedState<{
  model: Model;
  router: ConnRouter.RouterState<unknown>;
}>;

type MsgOrRouter = Msg | ConnRouter.RouterAction<unknown>;

const hist = createHashHistory();
const store: Redux.Store<ModelAndRouter, MsgOrRouter> = Redux.createStore(
  Redux.combineReducers({
    model: reducer,
    router: ConnRouter.connectRouter(hist),
  }),
  undefined,
  Redux.compose(Redux.applyMiddleware(ConnRouter.routerMiddleware(hist))),
);

const mountpoint = document.getElementById('react-mountpoint');
store.subscribe(() => {
  ReactDOM.render(routing(parsePage(store.getState().router)), mountpoint);
});

if (typeof window === 'object') {
  (window as any).hist = hist; //eslint-disable-line @typescript-eslint/no-explicit-any

  window.addEventListener('load', () => {
    // ??? Just connecting up the `hist` with the store with
    // some event listeners. I'd rather not render at all.
    ReactDOM.render(
      <ReactRedux.Provider store={store}>
        <ConnectedRouter history={hist}></ConnectedRouter>
      </ReactRedux.Provider>,
      document.getElementById('router-mountpoint'),
    );
    store.dispatch({ type: 'noop' });
  });
}
