'use strict';

import * as Redux from 'redux';
import ReactDOM from 'react-dom';
import { createHashHistory } from 'history';

import * as core from './core';
import * as router from './router';
import { view } from './views';
import { effectsOf } from './effects';
import { typecheckNever } from './utils/utils';

type Model = {
  core: core.Model;
  router: router.Model;
};

type Msg =
  | { type: 'core'; sub: core.Msg }
  | { type: 'router'; sub: router.Msg };

const liftMsg = {
  core(sub: core.Msg): Msg {
    return { type: 'core', sub };
  },
  router(sub: router.Msg): Msg {
    return { type: 'router', sub };
  },
};

type Dispatches = {
  core: Redux.Dispatch<core.Msg>;
  router: Redux.Dispatch<router.Msg>;
};

const splitDispatch = (dispatch: Redux.Dispatch<Msg>): Dispatches => ({
  core(m) {
    // Subtle!!
    // Cannot return `dispatch(liftMsg.core(m)).sub`, because once
    // data flows through the bigger `dispatch`, the returned bigger
    // `Msg` could very well be `router.Msg` from the other branch!
    dispatch(liftMsg.core(m));
    return m;
  },
  router(m) {
    dispatch(liftMsg.router(m));
    return m;
  },
});

const reduce = (init: Model) => (model: Model | undefined, msg: Msg): Model => {
  if (model === undefined) {
    return init;
  }
  switch (msg.type) {
    case 'core':
      return {
        ...model,
        core: core.reduce(init.core)(model.core, msg.sub),
      };
    case 'router':
      return {
        ...model,
        router: router.reduce(init.router)(model.router, msg.sub),
      };
    default:
      typecheckNever(msg);
      return model;
  }
};

const mountpoint = document.getElementById('react-mountpoint');

const hist = createHashHistory();

const init: Model = {
  core: core.init,
  router: { location: hist.location },
};

const store: Redux.Store<Model, Msg> = Redux.createStore(reduce(init));

const dispatches = splitDispatch(store.dispatch);

const render = () => {
  void ReactDOM.render(
    view(store.getState().router, {
      model: store.getState().core,
      dispatch: dispatches.core,
      effects: effectsOf(dispatches.core),
    }),
    mountpoint,
  );
};

const bootstrap = async () => {
  hist.listen(router.listener(dispatches.router));
  store.subscribe(render);
  render();
};

if (typeof window === 'object') {
  //eslint-disable-next-line @typescript-eslint/no-explicit-any
  (window as any).twit = {
    hist,
    store,
    router,
    core,
    get state() {
      return store.getState();
    },
    effects: effectsOf(dispatches.core),
  };
  window.addEventListener('load', bootstrap);
}
