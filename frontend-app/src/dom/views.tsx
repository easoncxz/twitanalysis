import React, { ReactElement, ReactFragment, FC } from 'react';
import type * as history from 'history';

import * as core from './core';
import * as effects from './effects';
import * as fetchFaves from './pages/fetch-faves';
import * as fetchMe from './pages/fetch-me';
import * as idbF from './pages/idb-fiddle';
import * as sendTweet from './pages/send-tweet';
import * as listManagement from './pages/list-management';
import * as serviceWorkerManagement from './pages/service-worker-management';
import * as router from './router';
import { ListManagement } from './pages/list-management';
import { typecheckNever, stringEnumValues } from './utils/utils';

type MyDispatch<T> = (_: T) => void;

type Props = {
  models: {
    core: core.Model;
    listManagement: listManagement.Model;
    router: router.Model;
    fetchFaves: fetchFaves.Model;
  };
  dispatches: {
    core: MyDispatch<core.Msg>;
    listManagement: MyDispatch<listManagement.Msg>;
    router: MyDispatch<router.Msg>;
    fetchFaves: MyDispatch<fetchFaves.Msg>;
  };
  effects: effects.Effects;
};

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
  location: history.Location<unknown>,
): Page | undefined {
  for (const page of stringEnumValues(Page)) {
    if (location.pathname === page) {
      return page;
    }
  }
  return undefined;
}

function viewContent(props: Props): ReactFragment {
  const { location } = props.models.router;
  const page = parseLocation(location);
  switch (page) {
    case Page.Home:
      return <p>Please click through the nav menu!</p>;
    case Page.FetchMe: {
      const dispatch = (sub: fetchMe.Msg) =>
        props.dispatches.core({
          type: 'fetch_me',
          sub,
        });
      return (
        <fetchMe.View
          model={props.models.core.user}
          dispatch={dispatch}
          effects={new fetchMe.Effects(dispatch)}
        />
      );
    }
    case Page.FetchFaves:
      return (
        <fetchFaves.View
          user={props.models.core.user}
          model={props.models.fetchFaves}
          dispatch={props.dispatches.fetchFaves}
        />
      );
    case Page.SendTweet:
      return (
        <>
          {sendTweet.View({
            model: props.models.core,
            dispatch: props.dispatches.core,
            effects: props.effects,
          })}
        </>
      );
    case Page.IndexDBFiddle:
      return <idbF.View />;
    case Page.ServiceWorkerManagement:
      return serviceWorkerManagement.view(props.dispatches.core, props.effects);
    case Page.ListManagement: {
      const smallProps: listManagement.Props = {
        model: props.models.listManagement,
        dispatch: (inner: listManagement.Msg) =>
          props.dispatches.listManagement(inner),
      };
      return <ListManagement props={smallProps} />;
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

export function view(props: Props): ReactElement {
  return (
    <div id="react-main-view">
      <h1>Welcome to TwitAnalysis</h1>
      <NavLinks />
      <div id="toast-box">
        {props.models.core.errors.map((e, i) => (
          <div
            key={`error-toast-${i}`}
            className="error-toast"
            onClick={() =>
              props.dispatches.core({ type: 'clear_error', error: e })
            }
          >
            <code>
              {e.name}: {e.message}
            </code>
            <pre>{e.stack}</pre>
          </div>
        ))}
      </div>
      <hr />
      {viewContent(props)}
    </div>
  );
}
