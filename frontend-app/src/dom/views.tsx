import React, { ReactElement, ReactFragment, FC } from 'react';

import * as core from './core';
import * as effects from './effects';
import * as fetchFaves from './pages/fetch-faves';
import * as fetchMe from './pages/fetch-me';
import * as idbF from './pages/idb-fiddle';
import * as listManagement from './pages/list-management';
import * as router from './router';
import { ListManagement } from './pages/list-management';
import { Page, parseLocation } from './core';
import { typecheckNever } from './utils/utils';

type MyDispatch<T> = (_: T) => void;

type Props = {
  models: {
    core: core.Model;
    listManagement: listManagement.Model;
    router: router.Model;
  };
  dispatches: {
    core: MyDispatch<core.Msg>;
    listManagement: MyDispatch<listManagement.Msg>;
    router: MyDispatch<router.Msg>;
  };
  effects: effects.Effects;
};

function viewSendTweet({ models, dispatches, effects }: Props): ReactElement {
  return (
    <div>
      <h2>Send tweet</h2>
      <form action="">
        <textarea
          value={models.core.pendingTweet}
          onChange={(e) => {
            dispatches.core({
              type: 'update_pending_tweet',
              text: e.target.value,
            });
          }}
          disabled={models.core.sendingTweet}
        ></textarea>
        <br />
        <pre>{models.core.pendingTweet}</pre>
        <input
          type="submit"
          onClick={(e) => {
            e.preventDefault();
            dispatches.core(effects.sendTweet(models.core.pendingTweet));
          }}
          disabled={models.core.sendingTweet}
          value="Send this tweet"
        />
      </form>
      <p>Sent tweets:</p>
      <ul>
        {models.core.sentTweets.map((st, i) => (
          <li key={`sentTweets-${i}`}>
            <code>{st.created_at}</code> - {st.text}
          </li>
        ))}
      </ul>
    </div>
  );
}

function viewServiceWorkerManagement(props: Props): ReactFragment {
  return (
    <>
      <button
        onClick={() =>
          props.dispatches.core(props.effects.registerServiceWorker())
        }
      >
        register ServiceWorker
      </button>
      <button
        onClick={() =>
          props.dispatches.core(props.effects.unregisterAllServiceWorkers())
        }
      >
        unregister all ServiceWorkers
      </button>
    </>
  );
}

function viewUnknown(): ReactElement {
  return (
    <p>
      Unknown route. You seem lost. <a href={'#' + Page.Home}>Go home</a>
    </p>
  );
}

function viewContent(props: Props): ReactFragment {
  const { location } = props.models.router;
  const page = parseLocation(location);
  switch (page) {
    case Page.Home:
      return <p>Please click through the nav menu!</p>;
    case Page.FetchMe:
      return (
        <fetchMe.View
          model={props.models.core}
          dispatch={props.dispatches.core}
          effects={props.effects}
        />
      );
    case Page.FetchFaves:
      return (
        <fetchFaves.View
          model={props.models.core}
          dispatch={props.dispatches.core}
          effects={props.effects}
        />
      );
    case Page.SendTweet:
      return viewSendTweet(props);
    case Page.IndexDBFiddle:
      return <idbF.View />;
    case Page.ServiceWorkerManagement:
      return viewServiceWorkerManagement(props);
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
