'use strict';

import * as Redux from 'redux';
import ReactDOM from 'react-dom';
import { createHashHistory, UnregisterCallback } from 'history';

import { Model, Msg, reduce, init, view, splitDispatch } from './views';
import * as router from './router';

const hist = createHashHistory<router.MyLocationState>();

const store: Redux.Store<Model, Msg> = Redux.createStore(
  reduce(init(hist.location)),
);

const mountpoint = document.getElementById('react-mountpoint');
const render = () =>
  void ReactDOM.render(view(store.getState(), store.dispatch), mountpoint);

let unlistenHist: UnregisterCallback | undefined;
const bootstrap = () => {
  const dispatches = splitDispatch(store.dispatch);
  unlistenHist = hist.listen(router.listener(dispatches.router));
  store.subscribe(render);
  render();
};
const teardown = () => {
  unlistenHist?.();
};

if (typeof window === 'object') {
  const { core, router } = store.getState();
  //eslint-disable-next-line @typescript-eslint/no-explicit-any
  (window as any).twit = {
    hist,
    store,
    router,
    core,
    get state() {
      return store.getState();
    },
  };
  window.addEventListener('load', bootstrap);
  window.addEventListener('unload', teardown);
}
