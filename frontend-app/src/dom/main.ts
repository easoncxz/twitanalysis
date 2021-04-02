'use strict';

import * as Redux from 'redux';
import ReactDOM from 'react-dom';
import { createHashHistory } from 'history';

import * as core from './core';
import * as router from './router';
import * as listManagement from './pages/list-management';
import { view } from './views';
import { effectsOf } from './effects';
import { typecheckNever } from './utils/utils';

type Model = {
  core: core.Model;
  router: router.Model;
  listManagement: listManagement.Model;
};

type Msg =
  | { type: 'core'; sub: core.Msg }
  | { type: 'router'; sub: router.Msg }
  | { type: 'list_management'; sub: listManagement.Msg };

const liftMsg = {
  core(sub: core.Msg): Msg {
    return { type: 'core', sub };
  },
  router(sub: router.Msg): Msg {
    return { type: 'router', sub };
  },
  listManagement(sub: listManagement.Msg): Msg {
    return { type: 'list_management', sub };
  },
};

type Dispatches = {
  core: Redux.Dispatch<core.Msg>;
  router: Redux.Dispatch<router.Msg>;
  listManagement: Redux.Dispatch<listManagement.Msg>;
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
  listManagement(m) {
    dispatch(liftMsg.listManagement(m));
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
    case 'list_management':
      return {
        ...model,
        listManagement: listManagement.reduce(listManagement.init)(
          model.listManagement,
          msg.sub,
        ),
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
  listManagement: listManagement.init,
};

const store: Redux.Store<Model, Msg> = Redux.createStore(reduce(init));

const dispatches = splitDispatch(store.dispatch);

const render = () => {
  console.log(`About to call ReactDOM.render...`);
  ReactDOM.render(
    view({
      models: store.getState(),
      dispatches,
      effects: effectsOf(dispatches.core),
    }),
    mountpoint,
    () => {
      console.log(
        `This is part of the 3rd-arg callback passed to ReactDOM.render`,
      );
    },
  );
  console.log(`ReactDOM.render returned.`);
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
