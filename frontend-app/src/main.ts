'use strict';

import * as Redux from 'redux';
import ReactDOM from 'react-dom';
import { createHashHistory } from 'history';

import * as core from './core';
import { view } from './views';
import * as routing from './routing';
import { typecheckNever } from './utils';

type Model = {
  core: core.Model;
  routing: routing.Model;
};

type Msg =
  | { type: 'core'; msg: core.Msg }
  | { type: 'routing'; msg: routing.Msg };

const liftMsg = {
  core(msg: core.Msg): Msg {
    return { type: 'core', msg };
  },
  routing(msg: routing.Msg): Msg {
    return { type: 'routing', msg };
  },
};

const subDispatch = (dispatch: (m: Msg) => void) => ({
  core(msg: core.Msg): void {
    return dispatch(liftMsg.core(msg));
  },
  routing(msg: routing.Msg): void {
    return dispatch(liftMsg.routing(msg));
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
        core: core.reduce(init.core)(model.core, msg.msg),
      };
    case 'routing':
      return {
        ...model,
        routing: routing.reduce(init.routing)(model.routing, msg.msg),
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
  routing: { location: hist.location },
};

const store: Redux.Store<Model, Msg> = Redux.createStore(reduce(init));

const render = () =>
  void ReactDOM.render(
    view(core.parseLocation(store.getState().routing.location)),
    mountpoint,
  );

const bootstrap = () => {
  hist.listen(routing.listener(subDispatch(store.dispatch).routing));
  store.subscribe(render);
  render();
};

if (typeof window === 'object') {
  //eslint-disable-next-line @typescript-eslint/no-explicit-any
  (window as any).twit = {
    hist,
    store,
    routing,
    core,
    get state() {
      return store.getState();
    },
    dispatch: subDispatch(store.dispatch),
  };
  window.addEventListener('load', bootstrap);
}
