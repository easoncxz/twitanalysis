import React, { ReactElement, ReactFragment } from 'react';
import type * as Redux from 'redux';

import { Model, Msg, Page, parseLocation } from './core';
import type { Effects } from './effects';
import * as Routing from './routing';
import { Status } from './twitter';
import * as F from './idb-fiddle';
import { pretty, typecheckNever } from './utils';

type Props = {
  model: Model;
  dispatch: Redux.Dispatch<Msg>;
  effects: Effects;
};

function viewFetchMe({ model, dispatch, effects }: Props): ReactElement {
  return (
    <div>
      <h1>Fetch own user</h1>
      {model.user ? (
        <div>
          <p>You are:</p>
          <pre>{pretty(model.user)}</pre>
        </div>
      ) : (
        <div>
          {model.fetchingMe ? <p>Fetching...</p> : null}
          <button
            onClick={() => dispatch(effects.fetchMe())}
            disabled={model.fetchingMe}
          >
            Tell me who I am
          </button>
        </div>
      )}
    </div>
  );
}

function viewFetchFaves({ model, dispatch, effects }: Props): ReactFragment {
  const go = () =>
    dispatch(
      effects.fetchFaves(model.user?.screen_name ?? model.faveNick, 200),
    );
  const faveList = model.faves
    .filter((f) => new RegExp(model.searchFaves ?? '.').exec(f.text))
    .map((f: Status, i: number) => <li key={'fave-' + String(i)}>{f.text}</li>);
  return (
    <>
      <div>
        <span>Fetch your favourited tweets</span>
        <input
          type="text"
          value={model.user?.screen_name ?? model.faveNick}
          onChange={(e) =>
            dispatch({ type: 'update_fave_nick', nick: e.target.value })
          }
        />
        <button onClick={go} disabled={model.fetchingFaves}>
          Fetch from Twitter
        </button>
        <button onClick={() => dispatch(effects.readAllTweets())}>
          Load from IndexedDB
        </button>
        <button onClick={() => dispatch(effects.putTweets(model.faves))}>
          Save to IndexedDB
        </button>
      </div>
      <div>
        <span>Search:</span>
        <input
          type="text"
          value={model.searchFaves ?? ''}
          onChange={(e) =>
            dispatch({ type: 'update_search_faves', search: e.target.value })
          }
        />
      </div>
      {faveList.length > 0 ? <ul>{faveList}</ul> : <p>No faves</p>}
    </>
  );
}

function viewSendTweet({ model, dispatch, effects }: Props): ReactElement {
  return (
    <div>
      <h2>Send tweet</h2>
      <form action="">
        <textarea
          value={model.pendingTweet}
          onChange={(e) => {
            dispatch({ type: 'update_pending_tweet', text: e.target.value });
          }}
          disabled={model.sendingTweet}
        ></textarea>
        <br />
        <pre>{model.pendingTweet}</pre>
        <input
          type="submit"
          onClick={(e) => {
            e.preventDefault();
            dispatch(effects.sendTweet(model.pendingTweet));
          }}
          disabled={model.sendingTweet}
          value="Send this tweet"
        />
      </form>
      <p>Sent tweets:</p>
      <ul>
        {model.sentTweets.map((st, i) => (
          <li key={`sentTweets-${i}`}>
            <code>{st.created_at}</code> - {st.text}
          </li>
        ))}
      </ul>
    </div>
  );
}

function viewIndexDBFiddle(_props: Props): ReactFragment {
  return (
    <>
      <p>Click the button to run the logic.</p>
      <button
        onClick={async () => {
          const db = await F.openMyDB();
          await F.insertSomeData(db, F.things);
          return db.close();
        }}
      >
        Insert some data
      </button>
      <button
        onClick={async () => {
          const db = await F.openMyDB();
          await F.clearTable(db);
          return db.close();
        }}
      >
        Clear the table
      </button>
    </>
  );
}

function viewServiceWorkerManagement(props: Props): ReactFragment {
  return (
    <>
      <button
        onClick={() => props.dispatch(props.effects.registerServiceWorker())}
      >
        register ServiceWorker
      </button>
      <button
        onClick={() =>
          props.dispatch(props.effects.unregisterAllServiceWorkers())
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

function viewContent({ location }: Routing.Model, props: Props): ReactFragment {
  const page = parseLocation(location);
  switch (page) {
    case Page.Home:
      return <p>Please click through the nav menu!</p>;
    case Page.FetchMe:
      return viewFetchMe(props);
    case Page.FetchFaves:
      return viewFetchFaves(props);
    case Page.SendTweet:
      return viewSendTweet(props);
    case Page.IndexDBFiddle:
      return viewIndexDBFiddle(props);
    case Page.ServiceWorkerManagement:
      return viewServiceWorkerManagement(props);
    case undefined:
      return viewUnknown();
    default:
      typecheckNever(page);
      throw new TypeError(`page: never = ${page}`);
  }
}

function viewOneLink([k, v]: [string, string]): ReactElement {
  return (
    <a className="nav-item" href={'#' + v} key={`nav-${k}`}>
      {k}
    </a>
  );
}

function viewLinks(): ReactElement[] {
  return Object.entries(Page).map(viewOneLink);
}

export function view(routing: Routing.Model, props: Props): ReactElement {
  return (
    <div id="react-main-view">
      <h1>Welcome to TwitAnalysis</h1>
      <nav>{viewLinks()}</nav>
      <div id="toast-box">
        {props.model.errors.map((e, i) => (
          <div
            key={`error-toast-${i}`}
            className="error-toast"
            onClick={() => props.dispatch({ type: 'clear_error', error: e })}
          >
            <code>
              {e.name}: {e.message}
            </code>
            <pre>{e.stack}</pre>
          </div>
        ))}
      </div>
      <hr />
      {viewContent(routing, props)}
    </div>
  );
}
