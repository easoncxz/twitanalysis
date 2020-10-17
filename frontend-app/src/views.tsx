import React from 'react';
import type * as Redux from 'redux';

import { Model, Msg, Page } from './core';
import type { Effects } from './effects';
import { pretty, typecheckNever } from './utils';

export const App: React.FunctionComponent<{
  model: Model;
  dispatch: Redux.Dispatch<Msg>;
  effects: Effects;
}> = ({ model, dispatch, effects }) => (
  <div>
    <h1>Hello from React</h1>

    <h1>Fetch own user</h1>
    {
      // Fetch user
      (() => {
        if (model.user) {
          return (
            <div>
              <p>You are:</p>
              <pre>{pretty(model.user)}</pre>
            </div>
          );
        } else {
          const b = (disabled: boolean) => (
            <button
              onClick={() => dispatch(effects.fetchMe())}
              disabled={disabled}
            >
              Tell me who I am
            </button>
          );
          if (model.fetchingMe) {
            return (
              <div>
                <p>Fetching...</p>
                {b(true)}
              </div>
            );
          } else {
            return b(false);
          }
        }
      })()
    }

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

export function view(page: Page): React.ReactElement {
  switch (page) {
    case Page.Home:
      return <p>Root</p>;
    case Page.FetchMe:
      return (
        <div>
          <p>Fetch me</p>
        </div>
      );
    case Page.SendTweet:
      return <p>send tweet</p>;
    case Page.Unknown:
      return <p>Unknown route.</p>;
    default:
      typecheckNever(page);
      throw new TypeError(`page: never = ${page}`);
  }
}
