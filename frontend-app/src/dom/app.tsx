import React, { ReactElement, ReactFragment, FC } from 'react';
import type * as history from 'history';

import * as core from './core';
import * as fetchFaves from './pages/fetch-faves';
import * as fetchMe from './pages/fetch-me';
import * as idbF from './pages/idb-fiddle';
import * as sendTweet from './pages/send-tweet';
import * as listManagement from './pages/list-management';
import * as serviceWorkerManagement from './pages/service-worker-management';
import * as router from './router';
import { ListManagement } from './pages/list-management';
import { typecheckNever, stringEnumValues } from './utils/utils';

type MyDispatch<T> = (_: T) => T;

export type Model = {
  core: core.Model;
  router: router.Model;
  listManagement: listManagement.Model;
  fetchFaves: fetchFaves.Model;
  sendTweet: sendTweet.Model;
};

export const init = (location: router.Location): Model => ({
  core: core.init,
  router: router.init(location),
  listManagement: listManagement.init,
  fetchFaves: fetchFaves.init,
  sendTweet: sendTweet.init,
});

export type Msg =
  | { type: 'core'; sub: core.Msg }
  | { type: 'router'; sub: router.Msg }
  | { type: 'list_management'; sub: listManagement.Msg }
  | { type: 'fetch_faves'; sub: fetchFaves.Msg }
  | { type: 'send_tweet'; sub: sendTweet.Msg };

export const reduce = (init: Model) => (
  model: Model | undefined,
  msg: Msg,
): Model => {
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
      // Can't just use an ordinary `(n: never) => never` function,
      // because Redux actually abuse our reducer function to run
      // their internal actions. We must return the model despite
      // semantically it's more sensible to throw an error.
      typecheckNever(msg);
      return model;
  }
};

export type Dispatches = {
  core: MyDispatch<core.Msg>;
  router: MyDispatch<router.Msg>;
  listManagement: MyDispatch<listManagement.Msg>;
  fetchFaves: MyDispatch<fetchFaves.Msg>;
  sendTweet: MyDispatch<sendTweet.Msg>;
};

// This shouldn't be exported, but here main.ts needs to access router.
export const splitDispatch = (dispatch: MyDispatch<Msg>): Dispatches => ({
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

function viewUnknown(): ReactElement {
  return (
    <p>
      Unknown route. You seem lost. <a href={'#' + Page.Home}>Go home</a>
    </p>
  );
}

export enum Page {
  Home = '/',
  FetchMe = '/fetch-me',
  SendTweet = '/send-tweet',
  FetchFaves = '/fetch-faves',
  IndexDBFiddle = '/idb',
  ServiceWorkerManagement = '/sw-mgmt',
  ListManagement = '/list-management',
}

export function parseLocation(
  location: history.Location<router.MyLocationState>,
): Page | undefined {
  for (const page of stringEnumValues(Page)) {
    if (location.pathname === page) {
      return page;
    }
  }
  return undefined;
}

function viewContent(model: Model, dispatches: Dispatches): ReactFragment {
  const { location } = model.router;
  const page = parseLocation(location);
  switch (page) {
    case Page.Home:
      return <p>Please click through the nav menu!</p>;
    case Page.FetchMe: {
      return (
        <fetchMe.View model={model.core.user} dispatch={dispatches.core} />
      );
    }
    case Page.FetchFaves:
      return (
        <fetchFaves.View
          user={model.core.user}
          model={model.fetchFaves}
          dispatch={dispatches.fetchFaves}
        />
      );
    case Page.SendTweet:
      return (
        <sendTweet.View
          model={model.sendTweet}
          dispatch={dispatches.sendTweet}
        />
      );
    case Page.IndexDBFiddle:
      return <idbF.View />;
    case Page.ServiceWorkerManagement:
      return serviceWorkerManagement.view();
    case Page.ListManagement: {
      return (
        <ListManagement
          model={model.listManagement}
          dispatch={dispatches.listManagement}
        />
      );
    }
    case undefined:
      return viewUnknown();
    default:
      typecheckNever(page);
      throw new TypeError(`page: never = ${page}`);
  }
}

const OneNavLink: FC<{ href: string; name: string }> = ({ href, name }) => {
  return (
    <a className="nav-item" href={href}>
      {name}
    </a>
  );
};

const NavLinks: FC = () => {
  return (
    <nav>
      {Object.entries(Page).map(([k, v]) => (
        <OneNavLink href={'#' + v} name={k} key={'nav-' + k} />
      ))}
    </nav>
  );
};

export function view(model: Model, dispatch: MyDispatch<Msg>): ReactElement {
  const dispatches = splitDispatch(dispatch);
  return (
    <div id="react-main-view">
      <h1>Welcome to TwitAnalysis</h1>
      <NavLinks />
      <div id="toast-box">
        {model.core.errors.map((e, i) => (
          <div
            key={`error-toast-${i}`}
            className="error-toast"
            onClick={() => dispatches.core({ type: 'remove_error', error: e })}
          >
            <code>
              {e.name}: {e.message}
            </code>
            <pre>{e.stack}</pre>
          </div>
        ))}
      </div>
      <hr />
      {viewContent(model, dispatches)}
    </div>
  );
}
