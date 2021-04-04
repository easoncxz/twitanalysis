import React from 'react';

import { t } from '../twitter/models';
import * as twitter from '../twitter/models';
import { RemoteData } from '../utils/remote-data';
import { fetchJson, typecheckNever } from '../utils/utils';
import * as remoteData from '../utils/remote-data';

export type Model = {
  sentTweets: twitter.Status[];
  pendingTweet: string;
  sendTweet: RemoteData<twitter.Status, Error>;
};

export const init: Model = {
  sentTweets: [],
  pendingTweet: '(initial)',
  sendTweet: { type: 'idle' },
};

export type Msg =
  | { type: 'network'; update: RemoteData<twitter.Status, Error> }
  | { type: 'update_pending_tweet'; text: string };

export const reduce = (init: Model) => (
  model: Model | undefined,
  msg: Msg,
): Model => {
  if (model === undefined) {
    return init;
  }
  switch (msg.type) {
    case 'network':
      switch (msg.update.type) {
        // special-case: not just do RemoteData, but also append it to our list
        case 'ok': {
          const status = msg.update.data;
          return {
            ...model,
            sendTweet: { type: 'ok', data: status },
            sentTweets: model.sentTweets.concat([status]),
          };
        }
        default:
          return {
            ...model,
            sendTweet: remoteData.reduce(model.sendTweet, msg.update),
          };
      }
    case 'update_pending_tweet':
      return {
        ...model,
        pendingTweet: msg.text,
      };
    default:
      typecheckNever(msg);
      return model;
  }
};

class Effects {
  constructor(private readonly dispatch: (_: Msg) => void) {}

  sendTweet(text: string): Msg {
    const body = new URLSearchParams();
    body.set('status', text);
    fetchJson(t('statuses/update'), {
      method: 'POST',
      headers: {
        'Content-Type': 'application/x-www-form-urlencoded',
      },
      body,
    })
      .then(twitter.parseStatus)
      .then(
        (data: twitter.Status) => {
          this.dispatch({ type: 'network', update: { type: 'ok', data } });
        },
        (e) =>
          this.dispatch({
            type: 'network',
            update: { type: 'error', error: e },
          }),
      );
    return { type: 'network', update: { type: 'loading' } };
  }
}

export const View: React.FC<{
  model: Model;
  dispatch: (_: Msg) => void;
}> = ({ model, dispatch }) => {
  const effects = new Effects(dispatch);
  return (
    <div>
      <h2>Send tweet</h2>
      <form action="">
        <textarea
          value={model.pendingTweet}
          onChange={(e) => {
            dispatch({
              type: 'update_pending_tweet',
              text: e.target.value,
            });
          }}
          disabled={model.sendTweet.type === 'loading'}
        ></textarea>
        <br />
        <pre>{model.pendingTweet}</pre>
        <input
          type="submit"
          onClick={(e) => {
            e.preventDefault();
            dispatch(effects.sendTweet(model.pendingTweet));
          }}
          disabled={model.sendTweet.type === 'loading'}
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
};
