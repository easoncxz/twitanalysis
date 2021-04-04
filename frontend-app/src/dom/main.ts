'use strict';

import * as Redux from 'redux';
import ReactDOM from 'react-dom';
import { createHashHistory } from 'history';

import * as core from './core';
import * as router from './router';
import * as listManagement from './pages/list-management';
import * as fetchFaves from './pages/fetch-faves';
import * as sendTweet from './pages/send-tweet';
import { view } from './views';
import { typecheckNever } from './utils/utils';

type Model = {
  core: core.Model;
  router: router.Model;
  listManagement: listManagement.Model;
  fetchFaves: fetchFaves.Model;
  sendTweet: sendTweet.Model;
};

type Msg =
  | { type: 'core'; sub: core.Msg }
  | { type: 'router'; sub: router.Msg }
  | { type: 'list_management'; sub: listManagement.Msg }
  | { type: 'fetch_faves'; sub: fetchFaves.Msg }
  | { type: 'send_tweet'; sub: sendTweet.Msg };

type Dispatches = {
  core: Redux.Dispatch<core.Msg>;
  router: Redux.Dispatch<router.Msg>;
  listManagement: Redux.Dispatch<listManagement.Msg>;
  fetchFaves: Redux.Dispatch<fetchFaves.Msg>;
  sendTweet: Redux.Dispatch<sendTweet.Msg>;
};

const splitDispatch = (dispatch: Redux.Dispatch<Msg>): Dispatches => ({
  core(sub) {
    // Subtle!!
    // Cannot return `dispatch({ type: 'core', sub })`, because once
    // data flows through the bigger `dispatch`, the returned bigger
    // `Msg` could very well be `router.Msg` from the other branch!
    dispatch({ type: 'core', sub });
    return sub;
  },
  router(sub) {
    dispatch({ type: 'router', sub });
    return sub;
  },
  listManagement(sub) {
    dispatch({ type: 'list_management', sub });
    return sub;
  },
  fetchFaves(sub) {
    dispatch({ type: 'fetch_faves', sub });
    return sub;
  },
  sendTweet(sub) {
    dispatch({ type: 'send_tweet', sub });
    return sub;
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
    case 'fetch_faves':
      return {
        ...model,
        fetchFaves: fetchFaves.reduce(model.core.user)(fetchFaves.init)(
          model.fetchFaves,
          msg.sub,
        ),
      };
    case 'send_tweet':
      return {
        ...model,
        sendTweet: sendTweet.reduce(sendTweet.init)(model.sendTweet, msg.sub),
      };
    default:
      typecheckNever(msg);
      return model;
  }
};

const mountpoint = document.getElementById('react-mountpoint');

const hist = createHashHistory<never>();

const init: Model = {
  core: core.init,
  router: { location: hist.location },
  listManagement: listManagement.init,
  fetchFaves: fetchFaves.init,
  sendTweet: sendTweet.init,
};

const store: Redux.Store<Model, Msg> = Redux.createStore(reduce(init));

const dispatches = splitDispatch(store.dispatch);

const render = () => {
  console.log(`About to call ReactDOM.render...`);
  ReactDOM.render(
    view({
      models: store.getState(),
      dispatches,
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
  };
  window.addEventListener('load', bootstrap);
}
