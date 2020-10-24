'use strict';

import * as Redux from 'redux';
import ReactDOM from 'react-dom';
import { createHashHistory } from 'history';

import * as core from './core';
import * as routing from './routing';
import { view } from './views';
import { effectsOf } from './effects';
import { typecheckNever } from './utils';

type Model = {
  core: core.Model;
  routing: routing.Model;
};

type Msg =
  | { type: 'core'; sub: core.Msg }
  | { type: 'routing'; sub: routing.Msg };

const liftMsg = {
  core(sub: core.Msg): Msg {
    return { type: 'core', sub };
  },
  routing(sub: routing.Msg): Msg {
    return { type: 'routing', sub };
  },
};

type Dispatches = {
  core: Redux.Dispatch<core.Msg>;
  routing: Redux.Dispatch<routing.Msg>;
};

const splitDispatch = (dispatch: Redux.Dispatch<Msg>): Dispatches => ({
  core(m) {
    // Subtle!!
    // Cannot return `dispatch(liftMsg.core(m)).sub`, because once
    // data flows through the bigger `dispatch`, the returned bigger
    // `Msg` could very well be `routing.Msg` from the other branch!
    dispatch(liftMsg.core(m));
    return m;
  },
  routing(m) {
    dispatch(liftMsg.routing(m));
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
    case 'routing':
      return {
        ...model,
        routing: routing.reduce(init.routing)(model.routing, msg.sub),
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

const dispatches = splitDispatch(store.dispatch);

const render = () => {
  void ReactDOM.render(
    view(store.getState().routing, {
      model: store.getState().core,
      dispatch: dispatches.core,
      effects: effectsOf(dispatches.core),
    }),
    mountpoint,
  );
};

const bootstrap = () => {
  hist.listen(routing.listener(dispatches.routing));
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
    effects: effectsOf(dispatches.core),
  };
  window.addEventListener('load', bootstrap);
}
