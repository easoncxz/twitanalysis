import React, { ReactElement, ReactFragment, FC } from 'react';
import type * as Redux from 'redux';

import { Model, Msg, Page, parseLocation } from './core';
import type { Effects } from './effects';
import * as Routing from './routing';
import { Status } from './twitter';
import * as F from './idb-fiddle';
import { pretty, typecheckNever } from './utils';
import { ListManagement } from './list-management';

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
      effects.fetchFaves(model.user?.screen_name ?? model.faveNick, {
        count: 200,
        max_id: model.faves.length
          ? model.faves[model.faves.length - 1].id_str
          : undefined,
      }),
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
    case Page.ListManagement:
      return <ListManagement props={props} />;
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
        <OneNavLink href={'#' + v} name={k} key={'nav-' + name} />
      ))}
    </nav>
  );
};

export function view(routing: Routing.Model, props: Props): ReactElement {
  return (
    <div id="react-main-view">
      <h1>Welcome to TwitAnalysis</h1>
      <NavLinks />
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
