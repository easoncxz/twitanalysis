'use strict';

import { Store, createStore } from 'redux';
import ReactDOM from 'react-dom';
import { createHashHistory } from 'history';

import { Model, Msg, reduce, init, view, splitDispatch } from './app';
import * as router from './router';

const hist = createHashHistory<router.MyLocationState>();
const store: Store<Model, Msg> = createStore(reduce(init(hist.location)));

const mountpoint = document.getElementById('react-mountpoint');
const render = () =>
  void ReactDOM.render(view(store.getState(), store.dispatch), mountpoint);

const bootstrap = () => {
  const dispatches = splitDispatch(store.dispatch);
  hist.listen(router.listener(dispatches.router));
  store.subscribe(render);
  render();
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
}
